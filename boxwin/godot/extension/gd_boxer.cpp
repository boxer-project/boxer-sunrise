#include "gd_boxer.h"
#include <godot_cpp/core/class_db.hpp>
#include <ecl/ecl.h>

using namespace godot;

extern "C" {
    void init_lib_BOXER_CORE_LISP(cl_object);
}

GDBoxer* the_gdboxer_node;
Node* main_boxer_node;

void GDBoxer::_bind_methods() {
    // Action is: 0 - press/MOUSE-DOWN 1 - click/MOUSE-CLICK 2 - release/MOUSE-UP 3 - double click/ MOUSE-DOUBLE-CLICK
    ClassDB::bind_method(D_METHOD("handle_mouse_input", "action", "row", "pos", "click", "bits", "area"), &GDBoxer::handle_mouse_input);
    ClassDB::bind_method(D_METHOD("handle_open_file", "path"), &GDBoxer::handle_open_file);

    ClassDB::bind_method(D_METHOD("startup_lisp"), &GDBoxer::startup_lisp);
    ClassDB::bind_method(D_METHOD("shutdown_lisp"), &GDBoxer::shutdown_lisp);

    ADD_SIGNAL(MethodInfo("boxer_insert_cha", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "ch"),
         PropertyInfo(Variant::INT, "cha_no")));
    ADD_SIGNAL(MethodInfo("boxer_delete_cha", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "cha_no")));
    ADD_SIGNAL(MethodInfo("boxer_delete_chas_between_cha_nos", PropertyInfo(Variant::OBJECT, "row"),
        PropertyInfo(Variant::INT, "strt_cha_no"), PropertyInfo(Variant::INT, "stop_cha_no")));
    ADD_SIGNAL(MethodInfo("boxer_point_location", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "cha_no")));
}

/*
 * Bound Methods
 */

void GDBoxer::toggle_box_type() {
//   PackedByteArray
}

void GDBoxer::handle_open_file(Variant path) {
    godot::String str_path = String(path);
    UtilityFunctions::print("\nhandle_open_file1.11: ", path, " hmm: ", str_path, " hmm2: ", str_path.utf8());

    cl_object handle_boxer_open_file_funname = ecl_make_symbol("GODOT-OPEN-FILE", "BOXER");
    cl_funcall(2, handle_boxer_open_file_funname, ecl_make_simple_base_string(str_path.ascii(), str_path.length()));
}

void GDBoxer::handle_mouse_input(int action, Variant boxer_row, int pos, int click, int bits, int area) {
    UtilityFunctions::print("\nGodot: handle_mouse_input");
    cl_object handle_boxer_mouse_funname = ecl_make_symbol("GODOT-HANDLE-MOUSE-INPUT", "BOXER");
    cl_funcall(7, handle_boxer_mouse_funname,
        ecl_make_fixnum(action),
        ((BoxerLispRef*) ((Object*) boxer_row))->boxer_obj,
        ecl_make_fixnum(pos),
        ecl_make_fixnum(click),
        ecl_make_fixnum(bits),
        ecl_make_fixnum(area));
}

/*
 * Functions and signals to send updates from Boxer -> Godot. These are made available to our lisp code.
 */

cl_object convert_godot_to_ecl(Variant value) {
    // UtilityFunctions::print("convert_godot_to_ecl: type: ", value.get_type(), " name: ", value.get_type_name(value.get_type()));
    int type = value.get_type();
    if (value.INT == type) {
        return ecl_make_fixnum((int)value);
    }
    else if (value.OBJECT == type) {
        return ((BoxerLispRef*) ((Object*) value))->boxer_obj;
    }
    else if (value.STRING == type) {
        godot::String str = String(value);
        // TODO does this handle unicode??? try and maybe switch to str.utf8()?
        return ecl_make_simple_base_string(str.ascii(), str.length());
    }
    else {
        return ECL_NIL;
    }
}

// Caller from common lisp will pass in a vector to fill
cl_object fetch_event_from_queue(cl_object event_arr) {
    Array next = main_boxer_node->call("fetch_event_from_queue");
    if (next.size() > 0) {
        for (int i = 0; i < next.size(); i++) {
            ecl_aset(event_arr, i, convert_godot_to_ecl(next[i]));
        }
    }
    else {
        ecl_aset(event_arr, 0, ecl_make_fixnum(0));
    }
    return event_arr;
}

cl_object lisp_boxer_point_location(cl_object row, cl_object cha_no) {
    Array togo = Array();
    togo.push_back(main_boxer_node);
    togo.push_back("_on_gd_boxer_boxer_point_location");
    togo.push_back(Variant((Object*) ecl_foreign_data_pointer_safe(row)));
    togo.push_back(Variant((int) ecl_fixnum(cha_no)));
    main_boxer_node->call("push_to_scene_queue", togo);
    return ECL_NIL;
}

/*
 * Straightforard creation/make versions of functions to call from Boxer -> Godot
 */

cl_object lisp_boxer_make_box(cl_object boxer_box) {
    BoxerLispRef* bbox = memnew(BoxerLispRef);
    bbox->boxer_obj = boxer_box;
    Object* godot_box = main_boxer_node->call("make_box", Variant((Object *) bbox));
    return ecl_make_foreign_data(ECL_NIL, 0, godot_box);
}

cl_object lisp_boxer_make_row(cl_object boxer_row) {
    BoxerLispRef* brow = memnew(BoxerLispRef);
    brow->boxer_obj = boxer_row;
    Object *godot_row = main_boxer_node->call("make_row", Variant((Object *) brow));
    return ecl_make_foreign_data(ECL_NIL, 0, godot_row);
}

cl_object lisp_boxer_make_turtle(cl_object boxer_turtle) {
    BoxerLispRef* bturtle = memnew(BoxerLispRef);
    bturtle->boxer_obj = boxer_turtle;
    Object *godot_turtle = main_boxer_node->call("make_turtle", Variant((Object *) bturtle));
    return ecl_make_foreign_data(ECL_NIL, 0, godot_turtle);
}


cl_object lisp_boxer_get_name_row(cl_object box) {
    UtilityFunctions::print("lisp_boxer_get_name_row\n");
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    Object* godot_name_row = godot_box->call("get_name_row");
    return ecl_make_foreign_data(ECL_NIL, 0, godot_name_row);
}

/*
 * PACKED BYTE ARRAYS
 */
cl_object lisp_boxer_make_packed_byte_array(cl_object size) {
    UtilityFunctions::print("Going to try and make a PACKED BYTE ARRAY...\n");
    PackedInt32Array *pba = memnew(PackedInt32Array);
    int arr_size = ecl_fixnum(size);
    pba->resize(arr_size);
    UtilityFunctions::print(" Made a PBA:");
    UtilityFunctions::print(pba->size());
    UtilityFunctions::print(pba);
    return ecl_make_foreign_data(ECL_NIL, 0, pba);
}

cl_object lisp_boxer_packed_byte_array_set(cl_object pbarray, cl_object index, cl_object new_byte) {
    PackedInt32Array *pba = (PackedInt32Array*) ecl_foreign_data_pointer_safe(pbarray);
    int b = ecl_fixnum(new_byte);
    int idx = ecl_fixnum(index);
    pba->set(idx, b);
    return ECL_NIL;
}

/*
 * Display Style Lists
 */

cl_object lisp_boxer_set_graphics_mode_p(cl_object box, cl_object enabled) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call_deferred("set_graphics_mode_p", (int) ecl_fixnum(enabled));
    return ECL_NIL;
}

/*
 * GRAPHICS SHEET BOXES
 */

cl_object lisp_boxer_set_graphics_sheet_background(cl_object box, cl_object red, cl_object green, cl_object blue, cl_object alpha) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call_deferred("set_background",
                    ecl_single_float(red), ecl_single_float(green), ecl_single_float(blue), ecl_single_float(alpha));
    return ECL_NIL;
}

cl_object lisp_boxer_set_graphics_sheet_draw_dims(cl_object box, cl_object width, cl_object height) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->set_deferred("draw_wid", (int) ecl_fixnum(width));
    godot_box->set_deferred("draw_hei", (int)ecl_fixnum(height));
    return ECL_NIL;
}

cl_object lisp_boxer_set_graphics_sheet_bit_array(cl_object box, cl_object width, cl_object height, cl_object pixmap_data) {
    Object *godot_box = Variant((Object *)ecl_foreign_data_pointer_safe(box));
    PackedInt32Array *pba = (PackedInt32Array *)ecl_foreign_data_pointer_safe(pixmap_data);
    godot_box->call_deferred("set_bit_array",
                    Variant((int)ecl_fixnum(width)),
                    Variant((int)ecl_fixnum(height)),
                    Variant(*pba));
    return ECL_NIL;
}

Variant convert_ecl_to_godot (cl_object value) {
    if (ECL_SINGLE_FLOAT_P(value)) {
        return Variant(ecl_single_float(value));
    }
    else if (ECL_DOUBLE_FLOAT_P(value)) {
        return Variant(ecl_double_float(value));
    }
    else if (ECL_LONG_FLOAT_P(value)) {
        return Variant((float)ecl_long_float(value));
    }
    else if (ECL_FIXNUMP(value)) {
        return Variant((int)ecl_fixnum(value));
    }
    else if (ECL_FOREIGN_DATA_P(value)) {
        return Variant((Object *)ecl_foreign_data_pointer_safe(value));
    }
    // The strings need to come before vector and other sequences, since they are also sequences.
    else if (ECL_BASE_STRING_P(value)) {
        char * name = ecl_base_string_pointer_safe (ecl_null_terminated_base_string(value));
        return Variant(name);
    }
    else if (ECL_EXTENDED_STRING_P(value)) {
        char * name = ecl_base_string_pointer_safe (ecl_null_terminated_base_string(value));
        return Variant(name);
    }
    else if (ECL_VECTORP(value)) {
        if (value->vector.fillp > 0 && ECL_SYMBOLP(ecl_aref1(value, 0)) &&
            strcmp((char*)cl_symbol_name(ecl_aref1(value, 0)), "RGB")) {
            float r = ecl_single_float(ecl_aref1(value, 1));
            float g = ecl_single_float(ecl_aref1(value, 2));
            float b = ecl_single_float(ecl_aref1(value, 3));
            float a = 1.0;
            if (value->vector.fillp > 4)
                a = ecl_single_float(ecl_aref1(value, 4));
            return Variant(Color(r, g, b, a));
        }
        else {
            // TODO - We don't need vectors for any other drawing commands, but we may want
            // this conversion routine for other operations eventually.
            return 0;
        }
    }
    else {
        UtilityFunctions::print("Couldn't figure out type:", ecl_t_of(value));
        return 0;
    }
}

/*
 * MISC
 */
cl_object lisp_boxer_set_property(cl_object box, cl_object prop_name, cl_object prop_value) {
    // TODO, should this be on the insert queue?
    Object *godot_box = Variant((Object *)ecl_foreign_data_pointer_safe(box));
    char * name = ecl_base_string_pointer_safe (ecl_null_terminated_base_string(prop_name));
    godot_box->set_deferred(name, convert_ecl_to_godot(prop_value));
    return ECL_NIL;
}

cl_object lisp_boxer_push_to_scene_queue(cl_object args_vector) {
    Array togo = Array();
    for (int i = 0; i < args_vector->vector.fillp; i++) {
        togo.push_back(convert_ecl_to_godot(ecl_aref1(args_vector, i)));
    }
    main_boxer_node->call("push_to_scene_queue", togo);
    return ECL_NIL;
}

cl_object lisp_boxer_push_with_main_node(cl_object args_vector) {
    Array togo = Array();
    togo.push_back(main_boxer_node);
    for (int i = 0; i < args_vector->vector.fillp; i++) {
        togo.push_back(convert_ecl_to_godot(ecl_aref1(args_vector, i)));
    }
    main_boxer_node->call("push_to_scene_queue", togo);
    return ECL_NIL;
}

// main_boxer_node, world_node, first_row_node
void GDBoxer::startup_lisp(Node* m_node, Node* world_node, Node* first_row_node) {
    cl_object result;

    UtilityFunctions::print("Hello from GD Boxer Ready 4.5");

    char * wow = static_cast<char*>(malloc(sizeof(char)));
    wow[0] = 65;
    cl_boot(1, &wow);

    ecl_init_module(NULL, init_lib_BOXER_CORE_LISP);

    //
    // Constructing
    //
    cl_object aux = ecl_make_symbol("GDBOXER-MAKE-BOX", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_box, 1);

    aux = ecl_make_symbol("FETCH-EVENT-FROM-QUEUE", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) fetch_event_from_queue, 1);
    // define_ecl_lisp_func("FETCH-EVENT-FROM-QUEUE", fetch_event_from_queue, 1);

    aux = ecl_make_symbol("GDBOXER-MAKE-ROW", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_row, 1);

    aux = ecl_make_symbol("GDBOXER-MAKE-TURTLE", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_turtle, 1);

    aux = ecl_make_symbol("GDBOXER-GET-NAME-ROW", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_get_name_row, 1);

    //
    // ROWS
    //

    aux = ecl_make_symbol("GDBOXER-POINT-LOCATION", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_point_location, 2);

    //
    // PACKED BYTE ARRAYS
    //
    aux = ecl_make_symbol("GDBOXER-MAKE-PACKED-BYTE-ARRAY", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_packed_byte_array, 1);

    aux = ecl_make_symbol("GDBOXER-PACKED-BYTE-ARRAY-SET", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_packed_byte_array_set, 3);

    //
    // Display Style Lists
    //
    aux = ecl_make_symbol("GDBOXER-SET-GRAPHICS-MODE-P", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_graphics_mode_p, 2);

    //
    // GRAPHICS SHEET BOXES
    //
    aux = ecl_make_symbol("GDBOXER-SET-GRAPHICS-SHEET-BACKGROUND", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_graphics_sheet_background, 5);

    aux = ecl_make_symbol("GDBOXER-SET-GRAPHICS-SHEET-DRAW-DIMS", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_graphics_sheet_draw_dims, 3);

    aux = ecl_make_symbol("GDBOXER-SET-GRAPHICS-SHEET-BIT-ARRAY", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_graphics_sheet_bit_array, 4);

    //
    // MISC
    //

    aux = ecl_make_symbol("GDBOXER-SET-PROPERTY", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_property, 3);

    aux = ecl_make_symbol("GDBOXER-CALL-GODOT", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_push_to_scene_queue, 1);

    aux = ecl_make_symbol("GDBOXER-CALL-GODOT-MAIN", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_push_with_main_node, 1);

    //
    // END SIGNAL SETUP
    //

    result = cl_eval(c_string_to_object("(bw::start-embedded-boxer nil)"));
    ecl_print(result, ECL_T);

    result = cl_eval(c_string_to_object("(load \"/Users/sgithens/code/boxer-sunrise/embedded/embedded-utils.lisp\")"));
    ecl_print(result, ECL_T);

    // Setup the Global node
    main_boxer_node = m_node;

    cl_object cl_world_node = ecl_make_foreign_data(ECL_NIL, 0, world_node);
    cl_object cl_first_row_node = ecl_make_foreign_data(ECL_NIL, 0, first_row_node);
    cl_object link_initial_box_to_node_fun_name = ecl_make_symbol("LINK_INITIAL_BOX_TO_NODE", "BOXER");
    result = cl_funcall(3, link_initial_box_to_node_fun_name, cl_world_node, cl_first_row_node);

    UtilityFunctions::print("Goodbye from GD Boxer Ready 2.7");
    UtilityFunctions::print("About to start boxer-command-loop-internal...");
    cl_object eval_loop = ecl_make_symbol("ECL-BOXER-COMMAND-LOOP-INTERNAL", "BOXER");
    cl_funcall(1, eval_loop);

    cl_shutdown();
}

void GDBoxer::shutdown_lisp() {
    cl_shutdown();
}

GDBoxer::GDBoxer() {
    the_gdboxer_node = this;
}

GDBoxer::~GDBoxer() {
}

// BoxerLispRef
BoxerLispRef::BoxerLispRef() {
}

BoxerLispRef::~BoxerLispRef() {
}

void BoxerLispRef::_bind_methods() {
}
