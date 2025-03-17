/**
 * HSynth CLAP Plugin - C integration layer
 * 
 * This file provides a bridge between the Haskell-implemented CLAP plugin
 * and C-based CLAP hosts. It re-exports the `clap_entry` symbol from Haskell
 * to ensure proper symbol visibility.
 */

#include <clap/entry.h>
#include <HsFFI.h>

/* 
 * Initialize Haskell runtime on library load 
 */
void __attribute__((constructor)) plugin_init(void) {
    hs_init(0, 0);
}

/*
 * Shutdown Haskell runtime on library unload
 */
void __attribute__((destructor)) plugin_fini(void) {
    hs_exit();
}

/* 
 * Reference to the clap_entry symbol from the Haskell side 
 * This is exported by the Haskell FFI foreign export.
 */
extern const clap_plugin_entry_t clap_entry;

/*
 * This is just for tracing/debugging when needed
 */
/* 
const char* get_plugin_id(void) {
    return clap_entry.get_factory("clap.plugin-factory");
}
*/ 