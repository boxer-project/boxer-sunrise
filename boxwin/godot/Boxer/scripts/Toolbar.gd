extends HBoxContainer

@onready var open_dialog: FileDialog = get_node("/root/Main/OpenFileDialog")

signal box_background_color_changed(color: Color)

func _on_size_item_selected(index: int) -> void:
    var font_size = $TextItems/Size.get_item_text(index).to_int()
    print("New font size: ", font_size)
    Global.cur_font_size = font_size


func _on_font_color_color_changed(color: Color) -> void:
    Global.cur_font_color = color


func _on_background_color_color_changed(color: Color) -> void:
    emit_signal("box_background_color_changed", color)


func _on_open_pressed() -> void:
    print("Open file2...")
    open_dialog.popup_centered()
