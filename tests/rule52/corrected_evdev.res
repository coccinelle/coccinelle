/*
 * $Id: corrected_evdev.res,v 1.1 2007-02-10 18:25:59 julia Exp $
 *
 *  Copyright (c) 1999-2001 Vojtech Pavlik
 *
 *  Event char devices, giving access to raw input device events.
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Should you need to contact me, the author, you can do so either by
 * e-mail - mail your message to <vojtech@ucw.cz>, or by paper mail:
 * Vojtech Pavlik, Simunkova 1594, Prague 8, 182 00 Czech Republic
 */

#define EVDEV_MINOR_BASE	64
#define EVDEV_MINORS		32
#define EVDEV_BUFFER_SIZE	64

#include <linux/poll.h>
#include <linux/slab.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/input.h>
#include <linux/smp_lock.h>

struct evdev {
	int exist;
	int open;
	int minor;
	char name[16];
	struct input_handle handle;
	wait_queue_head_t wait;
	devfs_handle_t devfs;
	struct evdev_list *list;
};

struct evdev_list {
	struct input_event buffer[EVDEV_BUFFER_SIZE];
	int head;
	int tail;
	struct fasync_struct *fasync;
	struct evdev *evdev;
	struct evdev_list *next;
};

static struct evdev *evdev_table[EVDEV_MINORS] = { NULL, /* ... */ };

static void evdev_event(struct input_handle *handle, unsigned int type, unsigned int code, int value)
{
	struct evdev *evdev = handle->private;
	struct evdev_list *list = evdev->list;

	while (list) {

		do_gettimeofday(&list->buffer[list->head].time);
		list->buffer[list->head].type = type;
		list->buffer[list->head].code = code;
		list->buffer[list->head].value = value;
		list->head = (list->head + 1) & (EVDEV_BUFFER_SIZE - 1);

		kill_fasync(&list->fasync, SIGIO, POLL_IN);

		list = list->next;
	}

	wake_up_interruptible(&evdev->wait);
}

static int evdev_fasync(int fd, struct file *file, int on)
{
	int retval;
	struct evdev_list *list = file->private_data;
	retval = fasync_helper(fd, file, on, &list->fasync);
	return retval < 0 ? retval : 0;
}

static int evdev_flush(struct file * file)
{
	struct evdev_list *list = (struct evdev_list*)file->private_data;
	if (!list->evdev->exist) return -ENODEV;
	return input_flush_device(&list->evdev->handle, file);
}

static int evdev_release(struct inode * inode, struct file * file)
{
	struct evdev_list *list = file->private_data;
	struct evdev_list **listptr;

	listptr = &list->evdev->list;
	evdev_fasync(-1, file, 0);

	while (*listptr && (*listptr != list))
		listptr = &((*listptr)->next);
	*listptr = (*listptr)->next;

	if (!--list->evdev->open) {
		if (list->evdev->exist) {
			input_close_device(&list->evdev->handle);
		} else {
			input_unregister_minor(list->evdev->devfs);
			evdev_table[list->evdev->minor] = NULL;
			kfree(list->evdev);
		}
	}

	kfree(list);

	return 0;
}

static int evdev_open(struct inode * inode, struct file * file)
{
	struct evdev_list *list;
	int i = minor(inode->i_rdev) - EVDEV_MINOR_BASE;
	int accept_err;

	if (i >= EVDEV_MINORS || !evdev_table[i])
		return -ENODEV;

	/* Ask the driver if he wishes to accept the open() */
	if ((accept_err = input_accept_process(&(evdev_table[i]->handle), file))) {
		return accept_err;
	}

	if (!(list = kmalloc(sizeof(struct evdev_list), GFP_KERNEL)))
		return -ENOMEM;
	memset(list, 0, sizeof(struct evdev_list));

	list->evdev = evdev_table[i];
	list->next = evdev_table[i]->list;
	evdev_table[i]->list = list;

	file->private_data = list;

	if (!list->evdev->open++)
		if (list->evdev->exist)
			input_open_device(&list->evdev->handle);

	return 0;
}

static ssize_t evdev_write(struct file * file, const char * buffer, size_t count, loff_t *ppos)
{
	struct evdev_list *list = file->private_data;
	struct input_event event;
	int retval = 0;

	if (!list->evdev->exist) return -ENODEV;

	while (retval < count) {

		if (copy_from_user(&event, buffer + retval, sizeof(struct input_event)))
			return -EFAULT;
		input_event(list->evdev->handle.dev, event.type, event.code, event.value);
		retval += sizeof(struct input_event);
	}

	return retval;
}

static ssize_t evdev_read(struct file * file, char * buffer, size_t count, loff_t *ppos)
{
	DECLARE_WAITQUEUE(wait, current);
	struct evdev_list *list = file->private_data;
	int retval = 0;

	if (list->head == list->tail) {

		add_wait_queue(&list->evdev->wait, &wait);
		set_current_state(TASK_INTERRUPTIBLE);

		while (list->head == list->tail) {

			if (!list->evdev->exist) {
				retval = -ENODEV;
				break;
			}
			if (file->f_flags & O_NONBLOCK) {
				retval = -EAGAIN;
				break;
			}
			if (signal_pending(current)) {
				retval = -ERESTARTSYS;
				break;
			}

			schedule();
		}

		set_current_state(TASK_RUNNING);
		remove_wait_queue(&list->evdev->wait, &wait);
	}

	if (retval)
		return retval;

	while (list->head != list->tail && retval + sizeof(struct input_event) <= count) {
		if (copy_to_user(buffer + retval, list->buffer + list->tail,
			 sizeof(struct input_event))) return -EFAULT;
		list->tail = (list->tail + 1) & (EVDEV_BUFFER_SIZE - 1);
		retval += sizeof(struct input_event);
	}

	return retval;
}

/* No kernel lock - fine */
static unsigned int evdev_poll(struct file *file, poll_table *wait)
{
	struct evdev_list *list = file->private_data;
	poll_wait(file, &list->evdev->wait, wait);
	if (list->head != list->tail)
		return POLLIN | POLLRDNORM;
	return 0;
}

static int evdev_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg)
{
	struct evdev_list *list = file->private_data;
	struct evdev *evdev = list->evdev;
	struct input_dev *dev = evdev->handle.dev;
	int retval, t, u;

	if (!evdev->exist) return -ENODEV;

	switch (cmd) {

		case EVIOCGVERSION:
			return put_user(EV_VERSION, (int *) arg);

		case EVIOCGID:
                        if ((retval = put_user(dev->id.bustype,     ((short *) arg) + 0))) return retval;
                         
                        if ((retval = put_user(dev->id.vendor,  ((short *) arg) + 1))) return retval;
			 
                        if ((retval = put_user(dev->id.product, ((short *) arg) + 2))) return retval;
			 
                        if ((retval = put_user(dev->id.version, ((short *) arg) + 3))) return retval;
			 
                        return 0;
		
		case EVIOCGREP:
			if ((retval = put_user(dev->rep[0], ((int *) arg) + 0))) return retval;
			if ((retval = put_user(dev->rep[1], ((int *) arg) + 1))) return retval;
			return 0;

		case EVIOCSREP:
			if ((retval = get_user(dev->rep[0], ((int *) arg) + 0))) return retval;
			if ((retval = get_user(dev->rep[1], ((int *) arg) + 1))) return retval;
			return 0;

		case EVIOCGKEYCODE:
			if ((retval = get_user(t, ((int *) arg) + 0))) return retval;
			if (t < 0 || t > dev->keycodemax) return -EINVAL;
			switch (dev->keycodesize) {
				case 1: u = *(u8*)(dev->keycode + t); break;
				case 2: u = *(u16*)(dev->keycode + t * 2); break;
				case 4: u = *(u32*)(dev->keycode + t * 4); break;
				default: return -EINVAL;
			}
			if ((retval = put_user(u, ((int *) arg) + 1))) return retval;
			return 0;

		case EVIOCSKEYCODE:
			if ((retval = get_user(t, ((int *) arg) + 0))) return retval;
			if (t < 0 || t > dev->keycodemax) return -EINVAL;
			if ((retval = get_user(u, ((int *) arg) + 1))) return retval;
			switch (dev->keycodesize) {
				case 1: *(u8*)(dev->keycode + t) = u; break;
				case 2: *(u16*)(dev->keycode + t * 2) = u; break;
				case 4: *(u32*)(dev->keycode + t * 4) = u; break;
				default: return -EINVAL;
			}
			return 0;

		case EVIOCSFF:
			if (dev->upload_effect) {
				struct ff_effect effect;
				int err;

				if (copy_from_user((void*)(&effect), (void*)arg, sizeof(effect))) {
					return -EFAULT;
				}
				err = dev->upload_effect(dev, &effect);
				if (put_user(effect.id, &(((struct ff_effect*)arg)->id))) {
					return -EFAULT;
				}
				return err;
			}
			else return -ENOSYS;

		case EVIOCRMFF:
			if (dev->erase_effect) {
				return dev->erase_effect(dev, (int)arg);
			}
			else return -ENOSYS;

		case EVIOCGEFFECTS:
			if ((retval = put_user(dev->ff_effects_max, (int*) arg)))
				return retval;
			return 0;

		default:

			if (_IOC_TYPE(cmd) != 'E' || _IOC_DIR(cmd) != _IOC_READ)
				return -EINVAL;

			if ((_IOC_NR(cmd) & ~EV_MAX) == _IOC_NR(EVIOCGBIT(0,0))) {

				long *bits;
				int len;

				switch (_IOC_NR(cmd) & EV_MAX) {
					case      0: bits = dev->evbit;  len = EV_MAX;  break;
					case EV_KEY: bits = dev->keybit; len = KEY_MAX; break;
					case EV_REL: bits = dev->relbit; len = REL_MAX; break;
					case EV_ABS: bits = dev->absbit; len = ABS_MAX; break;
					case EV_LED: bits = dev->ledbit; len = LED_MAX; break;
					case EV_SND: bits = dev->sndbit; len = SND_MAX; break;
					case EV_FF:  bits = dev->ffbit;  len = FF_MAX;  break;
					default: return -EINVAL;
				}
				len = NBITS(len) * sizeof(long);
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, bits, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGKEY(0))) {
				int len;
				len = NBITS(KEY_MAX) * sizeof(long);
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->key, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGLED(0))) {
				int len;
				len = NBITS(LED_MAX) * sizeof(long);
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->led, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGSND(0))) {
				int len;
				len = NBITS(SND_MAX) * sizeof(long);
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->snd, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGNAME(0))) {
				int len;
				if (!dev->name) return -ENOENT;
				len = strlen(dev->name) + 1;
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->name, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGPHYS(0))) {
				int len;
				if (!dev->phys) return -ENOENT;
				len = strlen(dev->phys) + 1;
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->phys, len) ? -EFAULT : len;
			}

			if (_IOC_NR(cmd) == _IOC_NR(EVIOCGUNIQ(0))) {
				int len;
				if (!dev->uniq) return -ENOENT;
				len = strlen(dev->uniq) + 1;
				if (len > _IOC_SIZE(cmd)) len = _IOC_SIZE(cmd);
				return copy_to_user((char *) arg, dev->uniq, len) ? -EFAULT : len;
			}

			if ((_IOC_NR(cmd) & ~ABS_MAX) == _IOC_NR(EVIOCGABS(0))) {

				int t = _IOC_NR(cmd) & ABS_MAX;

				if ((retval = put_user(dev->abs[t],     ((int *) arg) + 0))) return retval;
				if ((retval = put_user(dev->absmin[t],  ((int *) arg) + 1))) return retval;
				if ((retval = put_user(dev->absmax[t],  ((int *) arg) + 2))) return retval;
				if ((retval = put_user(dev->absfuzz[t], ((int *) arg) + 3))) return retval;
				if ((retval = put_user(dev->absflat[t], ((int *) arg) + 4))) return retval;

				return 0;
			}
	}
	return -EINVAL;
}

static struct file_operations evdev_fops = {
	owner:		THIS_MODULE,
	read:		evdev_read,
	write:		evdev_write,
	poll:		evdev_poll,
	open:		evdev_open,
	release:	evdev_release,
	ioctl:		evdev_ioctl,
	fasync:		evdev_fasync,
	flush:		evdev_flush
};

static struct input_handle *evdev_connect(struct input_handler *handler, struct input_dev *dev, struct input_device_id *id)
{
	struct evdev *evdev;
	int minor;

	for (minor = 0; minor < EVDEV_MINORS && evdev_table[minor]; minor++);
	if (minor == EVDEV_MINORS) {
		printk(KERN_ERR "evdev: no more free evdev devices\n");
		return NULL;
	}

	if (!(evdev = kmalloc(sizeof(struct evdev), GFP_KERNEL)))
		return NULL;
	memset(evdev, 0, sizeof(struct evdev));

	init_waitqueue_head(&evdev->wait);

	evdev->minor = minor;
	evdev_table[minor] = evdev;
	
	sprintf(evdev->name, "event%d", minor);

	evdev->handle.dev = dev;
	evdev->handle.name = evdev->name;
	evdev->handle.handler = handler;
	evdev->handle.private = evdev;

	evdev->devfs = input_register_minor("event%d", minor, EVDEV_MINOR_BASE);

	evdev->exist = 1;

	return &evdev->handle;
}

static void evdev_disconnect(struct input_handle *handle)
{
	struct evdev *evdev = handle->private;

	evdev->exist = 0;

	if (evdev->open) {
		input_close_device(handle);
		wake_up_interruptible(&evdev->wait);
	} else {
		input_unregister_minor(evdev->devfs);
		evdev_table[evdev->minor] = NULL;
		kfree(evdev);
	}
}

static struct input_device_id evdev_ids[] = {
	{ driver_info: 1 },	/* Matches all devices */
	{ },			/* Terminating zero entry */
};

MODULE_DEVICE_TABLE(input, evdev_ids);

static struct input_handler evdev_handler = {
	event:		evdev_event,
	connect:	evdev_connect,
	disconnect:	evdev_disconnect,
	fops:		&evdev_fops,
	minor:		EVDEV_MINOR_BASE,
	name:		"evdev",
	id_table:	evdev_ids,
};

static int __init evdev_init(void)
{
	input_register_handler(&evdev_handler);
	return 0;
}

static void __exit evdev_exit(void)
{
	input_unregister_handler(&evdev_handler);
}

module_init(evdev_init);
module_exit(evdev_exit);

MODULE_AUTHOR("Vojtech Pavlik <vojtech@ucw.cz>");
MODULE_DESCRIPTION("Input driver event char devices");
MODULE_LICENSE("GPL");
