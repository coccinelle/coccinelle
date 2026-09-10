int until_locked(struct drm_exec *exec, int *p)
{
	int ret = 0;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
	drm_exec_until_all_locked(exec) {
		ret = drm_exec_lock_obj(exec, p);
		drm_exec_retry_on_contention(exec);
	}
	return ret;
}

int compound_literal(struct ctx *ctx, int *p)
{
	int err;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
	xe_validation_guard(&ctx->v, &ctx->val, &ctx->exec, (struct xe_val_flags) {.interruptible = true},
			    err) {
		err = lock(ctx);
		drm_exec_retry_on_contention(&ctx->exec);
	}
	return err;
}

int long_arguments(struct ctx *ctx, int *p)
{
	int err;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
	xe_validation_guard(&ctx->v, &ctx->vm->xe->val, &ctx->exec,
			    ((struct xe_val_flags) {
				    .interruptible = true,
				    .exec_ignore_duplicates = true,
			    }), err) {
		err = lock(ctx);
	}
	return err;
}

int after_else(struct drm_exec *exec, int *p, int ret)
{
	p = kmalloc(sizeof(*p), GFP_KERNEL);
	if (ret)
		return ret;
	else
		unhinted_block_macro(exec) {
			ret = 1;
		}
	return ret;
}

/* The guess is not remembered: the same name used afterwards as an
 * ordinary function is still a call. */
int later_call(struct drm_exec *exec, int *p)
{
	int n;

	p = kmalloc(sizeof(*p), GFP_KERNEL);
	n = unhinted_block_macro(exec) + 1;
	return n;
}
