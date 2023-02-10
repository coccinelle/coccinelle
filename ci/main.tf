terraform {
  required_providers {
    cloudstack = {
      source  = "cloudstack/cloudstack"
      version = "0.4.0"
    }
  }
}

provider "cloudstack" {
  api_url = "https://sesi-cloud-ctl1.inria.fr/client/api"

  ## Provided by environment (Gitlab secrets)
  # api_key = "${var.cloudstack_api_key}"
  # secret_key = "${var.cloudstack_secret_key}"
}

variable "CI_PIPELINE_ID" {
  type = string
}

variable "REGISTRATION_TOKEN" {
  type      = string
  sensitive = true
}

variable "SSH_PUBLIC_KEY" {
  type = string
}

variable "runner_count" {
  type = number
}

resource "cloudstack_instance" "custom_instance" {
  count            = var.runner_count
  name             = "coccinelle-pipeline-${var.CI_PIPELINE_ID}"
  service_offering = "Custom"
  template         = "ubuntu-20.04-cloudinit"
  zone             = "zone-ci"
  details = {
    cpuNumber = 4
    memory    = 2048
  }
  expunge = true
  user_data = templatefile("cloud-init.sh.tftpl", {
    index              = count.index
    REGISTRATION_TOKEN = var.REGISTRATION_TOKEN
    SSH_PUBLIC_KEY     = var.SSH_PUBLIC_KEY
  })
  connection {
    type                = "ssh"
    host                = self.name
    user                = "ci"
    private_key         = file("id_rsa")
    bastion_host        = "ci-ssh.inria.fr"
    bastion_user        = "cci001"
    bastion_private_key = file("id_rsa")
  }
  provisioner "remote-exec" {
    when   = destroy
    inline = ["sudo gitlab-runner unregister --all-runners || true"]
  }
}
