import os
import cs

class CannotExtract(Exception):
    pass

def get_cloudstack_object(response, kind, index):
    try:
        if isinstance(response, list):
            # fetch_list directly returns the list
            return response[index]
        else:
            objects = response[kind]
            if isinstance(objects, list):
                return objects[index]
            else:
                # fetch_result directly returns the object
                return objects
    except KeyError:
        raise CannotExtract(f"Cannot extract {kind}[{index}] from {response}")

def main(cloudstack):
    vm_id = get_cloudstack_object(cloudstack.listVirtualMachines(
        name="coccinelle-pipeline-" + os.environ["CI_PARENT_PIPELINE_ID"],
        fetch_list=True
    ), "virtualmachine", 0)["id"]

    cloudstack.stopVirtualMachine(id=vm_id, fetch_result=True)

    volume_id = get_cloudstack_object(cloudstack.listVolumes(
        virtualmachineid=vm_id,
        fetch_list=True
    ), "volume", 0)["id"]

    os_type_id = get_cloudstack_object(cloudstack.listOsTypes(
        description="Ubuntu 20.04 LTS",
        fetch_list=True
    ), "ostype", 0)["id"]

    templates = cloudstack.listTemplates(
        templateFilter="self",
        name="coccinelle-trace-cmd",
        fetch_list=True
    )
    for template in templates:
        cloudstack.deleteTemplate(id=template["id"], fetch_result=True)

    template_id = get_cloudstack_object(cloudstack.createTemplate(
        displayText="coccinelle-trace-cmd",
        name="coccinelle-trace-cmd",
        volumeId=volume_id,
        osTypeId=os_type_id,
        fetch_result = True
    ), "template", 0)["id"]

    cloudstack.createTags(
        resourceType="template",
        resourceIds=template_id,
        **{
            "tags[0].key": "hash",
            "tags[0].value": os.environ["template_script_hash"]
        },
        fetch_result=True
    )

    cloudstack.startVirtualMachine(id=vm_id, fetch_result=True)

cloudstack = cs.CloudStack(
    endpoint="https://sesi-cloud-ctl1.inria.fr/client/api/",
    key=os.environ["CLOUDSTACK_API_KEY"],
    secret=os.environ["CLOUDSTACK_SECRET_KEY"]
)

main(cloudstack)
