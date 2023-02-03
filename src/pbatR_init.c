// tools::package_native_routine_registration_skeleton("pbatR")
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void addCommand(void *);
//extern void clearCommands();  // <<-- This is bad, in C, this means it has a variable # of args, it's not a real prototpe...
extern void clearCommands(void);
extern void kludgeConvert(void *, void *, void *);
extern void kludgeConvertAwful(void *, void *);
extern void launchPbatlog(void *, void *, void *, void *);
extern void launchPbatlogExtended(void *, void *, void *, void *);
extern void powerR(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
//extern void runCommands();
extern void runCommands(void);

static const R_CMethodDef CEntries[] = {
    {"addCommand",            (DL_FUNC) &addCommand,             1},
    {"clearCommands",         (DL_FUNC) &clearCommands,          0},
    {"kludgeConvert",         (DL_FUNC) &kludgeConvert,          3},
    {"kludgeConvertAwful",    (DL_FUNC) &kludgeConvertAwful,     2},
    {"launchPbatlog",         (DL_FUNC) &launchPbatlog,          4},
    {"launchPbatlogExtended", (DL_FUNC) &launchPbatlogExtended,  4},
    {"powerR",                (DL_FUNC) &powerR,                21},
    {"runCommands",           (DL_FUNC) &runCommands,            0},
    {NULL, NULL, 0}
};

void R_init_pbatR(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
