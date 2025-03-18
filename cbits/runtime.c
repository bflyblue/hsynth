/**
 * Common Haskell Runtime Initialization - Used by both the CLAP plugin and test
 * executable
 */

#include <HsFFI.h>
#include <stddef.h> /* For NULL */

/* Define the Haskell runtime initialization functions */
void initialize_haskell_runtime(void) {
  int argc = 1;
  char *argv[] = {"hsynth-clap", NULL}; // argv must end with NULL
  char **args = argv;
  hs_init(&argc, &args);
}

void finalize_haskell_runtime(void) { hs_exit(); }