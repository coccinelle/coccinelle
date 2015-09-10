#ifdef CONFIG_PPC_HAS_FEATURE_CALLS
#include <asm/pmac_feature.h>
#else
#include <asm/feature.h>
#endif

#define chip_t pmac_t

static int snd_pmac_register_sleep_notifier(pmac_t *chip);
static int snd_pmac_unregister_sleep_notifier(pmac_t *chip);
