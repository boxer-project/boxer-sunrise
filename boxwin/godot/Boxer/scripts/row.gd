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

func _on_gui_input(event: InputEvent) -> void:
    print("Row GUI Input: ", event)
