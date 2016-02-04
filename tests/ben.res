static GType this_type = 0;
static const GTypeInfo this_info = {
  sizeof(LassoProviderClass),
  NULL,
  NULL,
  (GClassInitFunc)class_init,
  NULL,
  NULL,
  sizeof(LassoProvider),
  0,
  (GInstanceInitFunc)instance_init,
  NULL,
};
GType
lasso_provider_get_type()
{
  if (!this_type) {
    this_type = g_type_register_static(LASSO_TYPE_NODE,
				       "LassoProvider", &this_info, 0);
  }
  return this_type;
}
