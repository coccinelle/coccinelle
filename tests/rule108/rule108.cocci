@@
struct usb_ctrlrequest *ctrl;
@@

(
  le16_to_cpu(ctrl->wIndex)
|
  le16_to_cpu(ctrl->wValue)
|
  le16_to_cpu(ctrl->wLength)
|
- ctrl->wIndex
+ le16_to_cpu(ctrl->wIndex)
|
- ctrl->wValue
+ le16_to_cpu(ctrl->wValue)
|
- ctrl->wLength
+ le16_to_cpu(ctrl->wLength)
)
