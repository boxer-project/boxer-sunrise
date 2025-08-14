#include <stdio.h>
#include <ecl/ecl.h>

int main (int argc, char **argv) {
    cl_boot(argc, argv);

    extern void init_lib_BOXER_CORE_LISP(cl_object);
    ecl_init_module(NULL, init_lib_BOXER_CORE_LISP);

    printf("Hello embedded Boxer\n");

    cl_object result = cl_eval(c_string_to_object("(load \"embedded-utils.lisp\")"));
    ecl_print(result, ECL_T);

    result = cl_eval(c_string_to_object("(boxer::wowwow)"));
    ecl_print(result, ECL_T);


    result = cl_eval(c_string_to_object("(bw::start-embedded-boxer nil)"));
    ecl_print(result, ECL_T);

    result = cl_eval(c_string_to_object("(bw::input-evaluation-demo)"));
    ecl_print(result, ECL_T);

    printf("Goodbye embedded Boxer\n");

    cl_shutdown();

    return 0;
}
