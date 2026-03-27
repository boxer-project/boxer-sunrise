#ifndef GDBOXER_H
#define GDBOXER_H

#ifdef BOXER_GDEXTENSION
#include <godot_cpp/classes/node.hpp>
#else
#include "scene/main/node.h"
#endif

#include <ecl/ecl.h>

#ifdef BOXER_GDEXTENSION
namespace godot {
#endif

class GDBoxer : public Node {
    GDCLASS(GDBoxer, Node)

// private:

protected:
    static void _bind_methods();

public:
    GDBoxer();
    ~GDBoxer();

    // void _ready() override;

    void handle_mouse_input(int action, Variant row, int pos, int click, int bits, int area);
    void handle_open_file(Variant path);
    void toggle_box_type();

    void startup_lisp(Node* m_node, Node* world_node, Node* first_row_node);
    void shutdown_lisp();

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

#ifdef BOXER_GDEXTENSION
}
#endif

#endif
