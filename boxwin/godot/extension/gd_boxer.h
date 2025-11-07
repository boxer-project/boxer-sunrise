#ifndef GDBOXER_H
#define GDBOXER_H

#include <godot_cpp/classes/node.hpp>
#include <ecl/ecl.h>

namespace godot {

class GDBoxer : public Node {
    GDCLASS(GDBoxer, Node)

// private:

protected:
    static void _bind_methods();

public:
    GDBoxer();
    ~GDBoxer();

    void _ready() override;

    void handle_character_input(int ch, int raw_bits);
    void handle_mouse_input(int action, Variant row, int pos, int click, int bits, int area);
    void handle_open_file(Variant path);
    void toggle_box_type();

    // void link_box_node_with_cl_box(Node);
};

class BoxerLispRef : public Node {
    GDCLASS(BoxerLispRef, Node)

// private:

protected:
    static void _bind_methods();

public:
    BoxerLispRef();
    ~BoxerLispRef();

    // void _ready() override;

    cl_object boxer_obj;
};

}

#endif
