/**
 * HSynth CLAP Plugin - C integration layer
 *
 * This file provides a bridge between the Haskell-implemented CLAP plugin
 * and C-based CLAP hosts. It re-exports the `clap_entry` symbol from Haskell
 * to ensure proper symbol visibility.
 */

#include "clap/clap.h"
#include <HsFFI.h>
#include <stddef.h> /* For NULL */

/* Windows-specific export directives */
#ifdef _WIN32
#define HSYNTH_EXPORT __declspec(dllexport)
#else
#define HSYNTH_EXPORT
#endif

/*
 * Reference to the clap_entry symbol from the Haskell side
 * This is exported by the Haskell FFI foreign export.
 *
 * We rename it to hs_clap_entry to avoid conflicts with the exported symbol
 */
extern const clap_plugin_entry_t hs_clap_entry;

/*
 * Create a pointer function that returns the Haskell entry point
 */
HSYNTH_EXPORT const clap_plugin_entry_t *clap_entry_getter(void) {
  return &hs_clap_entry;
}

#ifdef _WIN32
/* On Windows, we use the .def file to make clap_entry_getter accessible as
 * clap_entry */
#else
/* On other platforms, we can use a regular alias */
HSYNTH_EXPORT const clap_plugin_entry_t *clap_entry
    __attribute__((alias("clap_entry_getter")));
#endif

/*
 * This is just for tracing/debugging when needed
 */
/*
const char* get_plugin_id(void) {
    return clap_entry.get_factory("clap.plugin-factory");
}
*/