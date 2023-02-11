import hashlib
import os
import cs

with open("prepare-template.sh", "rb") as f:
    template_script_hash = hashlib.file_digest(f, "sha512").hexdigest()

cloudstack = cs.CloudStack(
    endpoint = "https://sesi-cloud-ctl1.inria.fr/client/api/",
    key = os.environ["CLOUDSTACK_API_KEY"],
    secret = os.environ["CLOUDSTACK_SECRET_KEY"]
)

def is_template_up_to_date(cloudstack, template_script_hash):
    try:
        templates = cloudstack.listTemplates(
            templateFilter = "self",
            name = "coccinelle-trace-cmd"
        )["template"]
    except KeyError:
        return False
    if len(templates) == 0:
        return False
    template = templates[0]
    try:
        tag = next(tag for tag in template["tags"] if tag["key"] == "hash")
    except StopIteration:
        return False
    return tag["value"] == template_script_hash

rebuild_template = not is_template_up_to_date(cloudstack, template_script_hash)

ci_pipeline_id = os.environ["CI_PIPELINE_ID"]

print(f"""
include: "ci/trace-cmd/pipeline.yml"
variables:
  template_script_hash: "{template_script_hash}"
  rebuild_template: "{rebuild_template}"
  CI_PARENT_PIPELINE_ID: "{ci_pipeline_id}"
.runner-tags:
  tags:
    - terraform
    - pipeline-{ci_pipeline_id}
""")
