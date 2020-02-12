# Configuration of the continuous integration system for Coccinelle

Continuous integration is activated for gitlab.inria.fr/coccinelle/coccinelle.
If you want to activate it for your own fork on gitlab, you just have to
follow the following steps:
- in the Settings/General page (https://gitlab.inria.fr/.../coccinelle/edit),
  expand the section "Visibility, project features, permissions" and
  activate "Pipelines", then "Save changes".
- in the new Settings/CI/CD page
  (https://gitlab.inria.fr/.../coccinelle/-/settings/ci_cd),
  expand the section "Runners" and
  look for "coccinelle-ubuntu-18-04" runner in the
  "Available specific runners" section
  (the readable text is in the description, not the title) and
  click on "Enable for this project".
