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
    ClassDB::bind_method(D_METHOD("handle_character_input", "ch", "bits"), &GDBoxer::handle_character_input);
    // Action is: 0 - press/MOUSE-DOWN 1 - click/MOUSE-CLICK 2 - release/MOUSE-UP 3 - double click/ MOUSE-DOUBLE-CLICK
    ClassDB::bind_method(D_METHOD("handle_mouse_input", "action", "row", "pos", "click", "bits", "area"), &GDBoxer::handle_mouse_input);
    ClassDB::bind_method(D_METHOD("handle_open_file", "path"), &GDBoxer::handle_open_file);

    ClassDB::bind_method(D_METHOD("toggle_box_type", "ch", "bits"), &GDBoxer::handle_character_input);

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

void GDBoxer::handle_character_input(int ch, int bits) {
    UtilityFunctions::print("\n3.handle_character_input: ", ch, " bits: ", bits, "\n");
    cl_object handle_boxer_input_funname = ecl_make_symbol("GODOT-HANDLE-BOXER-INPUT", "BOXER");
    cl_funcall(3, handle_boxer_input_funname, ecl_make_fixnum(ch), ecl_make_fixnum(bits));
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

cl_object lisp_boxer_insert_cha_signal(cl_object row, cl_object ch, cl_object cha_no)  {
    if ECL_FIXNUMP (ch)
    {
        // UtilityFunctions::print("lisp_boxer_insert_cha_signal: ", Variant((int) ecl_fixnum(ch)), " , ",
        //     Variant((int) ecl_fixnum(cha_no)));
        the_gdboxer_node->emit_signal("boxer_insert_cha",
            Variant((Object*) ecl_foreign_data_pointer_safe(row)),
            Variant((int) ecl_fixnum(ch)),
            Variant((int) ecl_fixnum(cha_no)));
    }
    else {
        the_gdboxer_node->emit_signal("boxer_insert_cha",
            Variant((Object*) ecl_foreign_data_pointer_safe(row)),
            Variant((Object*) ecl_foreign_data_pointer_safe(ch)),
            Variant((int) ecl_fixnum(cha_no)));
    }
    return ECL_NIL;
}

cl_object lisp_boxer_delete_cha_signal(cl_object row, cl_object cha_no)  {
    the_gdboxer_node->emit_signal("boxer_delete_cha",
        Variant((Object*) ecl_foreign_data_pointer_safe(row)),
        Variant((int) ecl_fixnum(cha_no)));
    return ECL_NIL;
}

cl_object lisp_boxer_delete_chas_between_cha_nos(cl_object row, cl_object strt_cha_no, cl_object stop_cha_no)  {
    the_gdboxer_node->emit_signal("boxer_delete_chas_between_cha_nos",
        Variant((Object*) ecl_foreign_data_pointer_safe(row)),
        Variant((int) ecl_fixnum(strt_cha_no)),
        Variant((int) ecl_fixnum(stop_cha_no)));
    return ECL_NIL;
}

cl_object lisp_boxer_insert_row_at_row_no(cl_object box, cl_object row, cl_object row_no) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("insert_row_at_row_no", Variant((Object*) ecl_foreign_data_pointer_safe(row)), Variant((int) ecl_fixnum(row_no)));
    return row;
}

cl_object lisp_boxer_delete_row_at_row_no(cl_object box, cl_object pos) {
    UtilityFunctions::print("lisp_boxer_delete_row_at_row_no\n");
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("delete_row_at_row_no", Variant((int) ecl_fixnum(pos)));
    return ECL_NIL;
}

cl_object lisp_boxer_point_location(cl_object row, cl_object cha_no) {
    // UtilityFunctions::print("\nlisp_boxer_point_location!!");
    the_gdboxer_node->emit_signal("boxer_point_location",
        Variant((Object*) ecl_foreign_data_pointer_safe(row)),
        Variant((int) ecl_fixnum(cha_no)));
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
    UtilityFunctions::print("Going to try and make a row6...\n");
    BoxerLispRef* brow = memnew(BoxerLispRef);
    brow->boxer_obj = boxer_row;
    Object *godot_row = main_boxer_node->call("make_row", Variant((Object *) brow));
    return ecl_make_foreign_data(ECL_NIL, 0, godot_row);
}


cl_object lisp_boxer_get_name_row(cl_object box) {
    UtilityFunctions::print("lisp_boxer_get_name_row\n");
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    Object* godot_name_row = godot_box->call("get_name_row");
    return ecl_make_foreign_data(ECL_NIL, 0, godot_name_row);
}

cl_object lisp_boxer_set_superior_box (cl_object row, cl_object superior_box) {
    Object* godot_row = Variant((Object*) ecl_foreign_data_pointer_safe(row));
    Object* godot_superior_box = Variant((Object*) ecl_foreign_data_pointer_safe(superior_box));
    godot_row->call("set_superior_box", godot_superior_box);
    return ECL_NIL;
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
 * BOXES
 */
cl_object lisp_boxer_toggle_to_data(cl_object box) {
    UtilityFunctions::print("lisp_boxer_toggle_to_data\n");
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("toggle_to_data");
    return ECL_NIL;
}

cl_object lisp_boxer_toggle_to_doit(cl_object box) {
    UtilityFunctions::print("lisp_boxer_toggle_to_doit\n");
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("toggle_to_doit");
    return ECL_NIL;
}

/*
 * Display Style Lists
 */

cl_object lisp_boxer_set_graphics_mode_p(cl_object box, cl_object enabled) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("set_graphics_mode_p", (int) ecl_fixnum(enabled));
    return ECL_NIL;
}

/*
 * GRAPHICS SHEET BOXES
 */

cl_object lisp_boxer_set_graphics_sheet_background(cl_object box, cl_object red, cl_object green, cl_object blue, cl_object alpha) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->call("set_background",
                    ecl_single_float(red), ecl_single_float(green), ecl_single_float(blue), ecl_single_float(alpha));
    return ECL_NIL;
}

cl_object lisp_boxer_set_graphics_sheet_draw_dims(cl_object box, cl_object width, cl_object height) {
    Object* godot_box = Variant((Object*) ecl_foreign_data_pointer_safe(box));
    godot_box->set("draw_wid", (int) ecl_fixnum(width));
    godot_box->set("draw_hei", (int)ecl_fixnum(height));
    return ECL_NIL;
}

cl_object lisp_boxer_set_graphics_sheet_bit_array(cl_object box, cl_object width, cl_object height, cl_object pixmap_data) {
    Object *godot_box = Variant((Object *)ecl_foreign_data_pointer_safe(box));
    PackedInt32Array *pba = (PackedInt32Array *)ecl_foreign_data_pointer_safe(pixmap_data);
    godot_box->call("set_bit_array",
                    Variant((int)ecl_fixnum(width)),
                    Variant((int)ecl_fixnum(height)),
                    Variant(*pba));
    return ECL_NIL;
}

/*
 * MISC
 */
cl_object lisp_boxer_set_property(cl_object box, cl_object prop_name, cl_object prop_value) {
    UtilityFunctions::print("C++ lisp_boxer_set_property\n");
    Object *godot_box = Variant((Object *)ecl_foreign_data_pointer_safe(box));
    UtilityFunctions::print("C++ lisp_boxer_set_property 2\n");
    // StringName *godot_prop_name = (StringName *)ecl_foreign_data_pointer_safe(prop_name);
    char * name = ecl_base_string_pointer_safe (ecl_null_terminated_base_string(prop_name));
    UtilityFunctions::print("C++ lisp_boxer_set_property 3\n");
    // godot_box->set("display_style", (int)ecl_fixnum(prop_value));
    godot_box->set(name, (int)ecl_fixnum(prop_value));
    UtilityFunctions::print("C++ lisp_boxer_set_property 4\n");
    return ECL_NIL;
}


void GDBoxer::_ready() {
    cl_object result;

    UtilityFunctions::print("Hello from GD Boxer Ready 4.5");

    char * wow = static_cast<char*>(malloc(sizeof(char)));
    wow[0] = 65;
    cl_boot(1, &wow);

    ecl_init_module(NULL, init_lib_BOXER_CORE_LISP);

    //
    // setup signal and other common lisp -> C++ calls
    //

    //
    // Constructing
    //
    cl_object aux = ecl_make_symbol("GDBOXER-MAKE-BOX", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_box, 1);

    aux = ecl_make_symbol("GDBOXER-MAKE-ROW", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_row, 1);

    aux = ecl_make_symbol("GDBOXER-GET-NAME-ROW", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_get_name_row, 1);

    //
    // CHAS
    //
    aux = ecl_make_symbol("GDBOXER-INSERT-CHA-SIGNAL", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_insert_cha_signal, 3);

    aux = ecl_make_symbol("GDBOXER-DELETE-CHA-SIGNAL", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_delete_cha_signal, 2);

    aux = ecl_make_symbol("GDBOXER-DELETE-CHAS-BETWEEN-CHA-NOS-SIGNAL", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_delete_chas_between_cha_nos, 3);

    //
    // ROWS
    //

    aux = ecl_make_symbol("GDBOXER-SET-SUPERIOR-BOX", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_superior_box, 2);

    aux = ecl_make_symbol("GDBOXER-INSERT-ROW-AT-ROW-NO", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_insert_row_at_row_no, 3);

    aux = ecl_make_symbol("GDBOXER-POINT-LOCATION", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_point_location, 2);

    aux = ecl_make_symbol("GDBOXER-DELETE-ROW-AT-ROW-NO", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_delete_row_at_row_no, 2);

    //
    // PACKED BYTE ARRAYS
    //
    aux = ecl_make_symbol("GDBOXER-MAKE-PACKED-BYTE-ARRAY", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_make_packed_byte_array, 1);

    aux = ecl_make_symbol("GDBOXER-PACKED-BYTE-ARRAY-SET", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_packed_byte_array_set, 3);


    //
    // BOXES
    //
    aux = ecl_make_symbol("GDBOXER-TOGGLE-TO-DATA", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_toggle_to_data, 1);

    aux = ecl_make_symbol("GDBOXER-TOGGLE-TO-DOIT", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_toggle_to_doit, 1);

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

    aux = ecl_make_symbol("GDBOXER-SET-PROPERTY", "BOXER");
    ecl_def_c_function(aux, (cl_objectfn_fixed) lisp_boxer_set_property, 3);



    //
    // END SIGNAL SETUP
    //

    // litmus test
    // result = cl_eval(c_string_to_object("(boxer::wowwow)"));
    // ecl_print(result, ECL_T);

    result = cl_eval(c_string_to_object("(bw::start-embedded-boxer nil)"));
    ecl_print(result, ECL_T);

    result = cl_eval(c_string_to_object("(load \"/Users/sgithens/code/boxer-sunrise/embedded/embedded-utils.lisp\")"));
    ecl_print(result, ECL_T);

    // Setup the Global node
    main_boxer_node = get_node<Node>("/root/Main");

    // Bootstrap the *initial-box* and it's first row
    Node *world_node = get_node<Node>("/root/Main/TopLevelContainer/OutermostBoxScroll/World");
    // UtilityFunctions::print("\nDoes this really worksBBB?: ", ClassDB::class_exists(world_node::get_class_static()));
    Node *first_row_node = get_node<Node>("/root/Main/TopLevelContainer/OutermostBoxScroll/World/BoxInternals/OuterBorderPanel/BoxPanel/PanelContainer/RowsBox/Row");
    cl_object cl_world_node = ecl_make_foreign_data(ECL_NIL, 0, world_node);
    cl_object cl_first_row_node = ecl_make_foreign_data(ECL_NIL, 0, first_row_node);
    cl_object link_initial_box_to_node_fun_name = ecl_make_symbol("LINK_INITIAL_BOX_TO_NODE", "BOXER");
    result = cl_funcall(3, link_initial_box_to_node_fun_name, cl_world_node, cl_first_row_node);

    result = cl_eval(c_string_to_object("(bw::type-stuff \"123\")"));

    UtilityFunctions::print("Goodbye from GD Boxer Ready 2.7");
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
    // ClassDB::bind_method(D_METHOD("handle_character_input", "ch", "bits"), &GDBoxer::handle_character_input);
    // ClassDB::bind_method(D_METHOD("handle_mouse_input", "row", "pos", "click", "bits", "area"), &GDBoxer::handle_mouse_input);

    // ClassDB::bind_method(D_METHOD("toggle_box_type", "ch", "bits"), &GDBoxer::handle_character_input);

    // ADD_SIGNAL(MethodInfo("boxer_insert_cha", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "ch"),
    //      PropertyInfo(Variant::INT, "cha_no")));
    // ADD_SIGNAL(MethodInfo("boxer_delete_cha", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "cha_no")));
    // ADD_SIGNAL(MethodInfo("boxer_delete_chas_between_cha_nos", PropertyInfo(Variant::OBJECT, "row"),
    //     PropertyInfo(Variant::INT, "strt_cha_no"), PropertyInfo(Variant::INT, "stop_cha_no")));
    // ADD_SIGNAL(MethodInfo("boxer_point_location", PropertyInfo(Variant::OBJECT, "row"), PropertyInfo(Variant::INT, "cha_no")));
}
