extends HBoxContainer

# Args: Parent row, cha (self), and position
signal cha_inserted

var parent_box
# Reference to the actual boxer row object in common lisp
var boxer_row

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    pass

func remove_cha(index) -> void:
    var cha = get_child(index)
    remove_child(cha)

func remove_chas(start_index, stop_index) -> void:
    for i in range(stop_index - start_index):
        remove_cha(start_index)

func add_cha(cha, idx: int) -> int:
    if cha.get_parent():
        print("Removing cha: ", cha, " from parent")
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
    cha_inserted.emit(self, cha, idx)
    return idx

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
        print("Row GUI Input: ", event)
        if get_child_count() == 0:
            $/root/Main.handle_mouse_input(0, boxer_row, 0, 0, 0, 0)
        elif sentence_end_xpos() < event.position.x:
            # Is this past the last character?
            $/root/Main.handle_mouse_input(0, boxer_row, get_child_count(), 0, 0, 0)
        elif get_child(0).position.x > event.position.x:
            # Is this before the first character?
            $/root/Main.handle_mouse_input(0, boxer_row, 0, 0, 0, 0)
        else:
            #loop through the chas
            for child in get_children():
                if xpos_in_cha(event.position.x, child):
                    $/root/Main.handle_mouse_input(0, boxer_row, child.get_index()+1, 0, 0, 0)

