#include "HsFFI.h"
#include "clap/clap.h"

extern bool pluginInit(const char *plugin_path);
extern void pluginDeinit(void);
extern const void *pluginGetFactory(const char *factory_id);

bool plugin_init_wrapper(const char *plugin_path) {
  hs_init(NULL, NULL);
  return pluginInit(plugin_path);
}

void plugin_deinit_wrapper(void) {
  pluginDeinit();
  hs_exit();
}

CLAP_EXPORT const clap_plugin_entry_t clap_entry = {
    .clap_version = CLAP_VERSION_INIT,
    .init = plugin_init_wrapper,
    .deinit = plugin_deinit_wrapper,
    .get_factory = pluginGetFactory,
};
