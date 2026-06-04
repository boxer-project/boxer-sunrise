extends HBoxContainer

var parent_box
# Reference to the actual boxer row object in common lisp
var boxer_row

@export var cha_scene: PackedScene

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    pass

func remove_cha(index) -> void:
    var cha = get_child(index)
    remove_child(cha)

func remove_chas(start_index, stop_index) -> void:
    for i in range(stop_index - start_index):
        remove_cha(start_index)

func sync_row_size(count: int) -> void:
    # Some operations in Boxer lisp update the row by setting the size of the
    # vector, which may be assumed to delete cha's from the end of it.
    var children = get_children()
    # TODO check to see that count <= current size
    for i in range(children.size() - count):
        var child = children[children.size() - 1 - i]
        child.queue_free()

func make_cha_scene(ch):
    var cha = cha_scene.instantiate()
    cha.text = ch
    return cha

func set_cha(ch, idx: int) -> int:
    var cha
    if typeof(ch) == TYPE_INT:
        cha = make_cha_scene(String.chr(ch))
    else:
        cha = ch

    # This function actually sets the cha, replacing the current cha at this index
    if cha.get_parent():
        var parent_row = cha.get_parent()
        parent_row.remove_child(cha)

    # This function is really going to "set" cha, so we need to
    # remove the cha currently at idx
    if idx < get_child_count():
        var cha_to_remove = get_child(idx)
        remove_child(cha_to_remove)

    # Adds a cha (Glyph or Box) to the row's chas
    add_child(cha)
    move_child(cha, idx)
    return idx

###
###  Fast Cha Array Set and Char Sliding (see infsup.lisp)
###
func slide_chas_pos(start, distance):
    for i in distance:
        var cha = $/root/Main.make_cha_scene(" ")
        add_child(cha)
        move_child(cha, start)

func slide_chas_neg(start, distance):
    pass

# For C++
func set_superior_box(box):
    parent_box = box

func sentence_end_xpos():
    "Finds the position of the end of the current sentence."
    if get_child_count() == 0:
        return 0
    var last_child = get_child(get_child_count()-1)
    return last_child.position.x + last_child.size.x

func xpos_in_cha(x, cha_node):
    return x >= cha_node.position.x && x <= cha_node.position.x + cha_node.size.x

func _on_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.is_pressed():
        if get_child_count() == 0:
            Global.handle_mouse_input(event, self, 0, Global.BoxArea.INSIDE)
        elif sentence_end_xpos() < event.position.x:
            # Is this past the last character?
            Global.handle_mouse_input(event, self, get_child_count(), Global.BoxArea.INSIDE)
        elif get_child(0).position.x > event.position.x:
            # Is this before the first character?
            Global.handle_mouse_input(event, self, 0, Global.BoxArea.INSIDE)
        else:
            #loop through the chas
            for child in get_children():
                if xpos_in_cha(event.position.x, child):
                    Global.handle_mouse_input(event, self, child.get_index()+1, Global.BoxArea.INSIDE)
