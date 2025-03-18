/**
 * HSynth CLAP Plugin - C integration layer
 *
 * This file provides a bridge between the Haskell-implemented CLAP plugin
 * and C-based CLAP hosts. It re-exports the `clap_entry` symbol from Haskell
 * to ensure proper symbol visibility.
 */

#include "clap/clap.h"
#include <HsFFI.h>
#include <stdio.h>
#include <stdlib.h>

/* Windows-specific headers */
#ifdef _WIN32
#include <windows.h>

/*
 * DllMain - entry point for Windows DLLs
 * This will be called before any other code in the DLL, including
 * our constructor functions
 */
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) {
  char logPath[MAX_PATH];
  FILE *fp;

  // Create a log in the user's document folder
  ExpandEnvironmentStrings("%USERPROFILE%\\hsynth_early_debug.log", logPath,
                           MAX_PATH);

  switch (fdwReason) {
  case DLL_PROCESS_ATTACH:
    fp = fopen(logPath, "w");
    if (fp) {
      fprintf(fp, "DLL_PROCESS_ATTACH: Process ID: %lu\n",
              GetCurrentProcessId());
      fprintf(fp, "DLL load path: %s\n",
              getenv("PATH") ? getenv("PATH") : "PATH not available");
      fprintf(fp, "DLL instance: %p\n", (void *)hinstDLL);
      fclose(fp);
    }
    break;

  case DLL_THREAD_ATTACH:
    fp = fopen(logPath, "a");
    if (fp) {
      fprintf(fp, "DLL_THREAD_ATTACH\n");
      fclose(fp);
    }
    break;

  case DLL_THREAD_DETACH:
    fp = fopen(logPath, "a");
    if (fp) {
      fprintf(fp, "DLL_THREAD_DETACH\n");
      fclose(fp);
    }
    break;

  case DLL_PROCESS_DETACH:
    fp = fopen(logPath, "a");
    if (fp) {
      fprintf(fp, "DLL_PROCESS_DETACH\n");
      fclose(fp);
    }
    break;
  }

  return TRUE;
}
#endif

/* Windows-specific export directives */
#ifdef _WIN32
#define HSYNTH_EXPORT __declspec(dllexport)
#else
#define HSYNTH_EXPORT
#endif

/*
 * Initialize Haskell runtime on library load
 */
void __attribute__((constructor)) plugin_init(void) {
  char logPath[MAX_PATH];
  FILE *fp = NULL;

  /* Try multiple locations for logging */
  fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "w");
  if (!fp) {
    fp = fopen("C:\\hsynth_debug.log", "w");
  }
  if (!fp) {
    ExpandEnvironmentStrings("%USERPROFILE%\\hsynth_debug.log", logPath,
                             MAX_PATH);
    fp = fopen(logPath, "w");
  }

  if (fp) {
    fprintf(fp, "[%lu] HSynth CLAP plugin initialization started\n",
            GetCurrentProcessId());
    fprintf(fp, "DLL load path: %s\n",
            getenv("PATH") ? getenv("PATH") : "PATH not available");
    fclose(fp);
  }

  /* Initialize with minimal options to avoid crashes */
  int argc = 0;
  char **argv = NULL;

  /* Try to log again */
  fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "a");
  if (!fp) {
    fp = fopen("C:\\hsynth_debug.log", "a");
  }
  if (!fp) {
    ExpandEnvironmentStrings("%USERPROFILE%\\hsynth_debug.log", logPath,
                             MAX_PATH);
    fp = fopen(logPath, "a");
  }

  if (fp) {
    fprintf(fp, "About to call hs_init\n");
    fclose(fp);
  }

  /* Try to initialize Haskell runtime */
  hs_init(&argc, &argv);

  /* Log successful initialization */
  fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "a");
  if (!fp) {
    fp = fopen("C:\\hsynth_debug.log", "a");
  }
  if (!fp) {
    ExpandEnvironmentStrings("%USERPROFILE%\\hsynth_debug.log", logPath,
                             MAX_PATH);
    fp = fopen(logPath, "a");
  }

  if (fp) {
    fprintf(fp, "Haskell runtime initialized successfully\n");
    fclose(fp);
  }
}

/*
 * Shutdown Haskell runtime on library unload
 */
void __attribute__((destructor)) plugin_fini(void) {
  FILE *fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "a");
  if (fp) {
    fprintf(fp, "Shutting down Haskell runtime\n");
    fclose(fp);
  }
  hs_exit();
}

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
  FILE *fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "a");
  if (fp) {
    fprintf(fp,
            "clap_entry_getter called, returning &hs_clap_entry (addr: %p)\n",
            (void *)&hs_clap_entry);
    fclose(fp);
  }
  return &hs_clap_entry;
}

#ifdef _WIN32
/* On Windows, we directly create the symbol with the correct name */
/* We don't define clap_entry directly here, as it causes a type conflict.
   Instead, we use the .def file to make clap_entry_getter accessible as
   clap_entry */

/* Initialization function to log when we're loaded */
void __attribute__((constructor)) clap_entry_init(void) {
  FILE *fp = fopen("C:\\Windows\\Temp\\hsynth_debug.log", "a");
  if (fp) {
    fprintf(fp, "clap_entry_getter initialization\n");
    fclose(fp);
  }
}
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