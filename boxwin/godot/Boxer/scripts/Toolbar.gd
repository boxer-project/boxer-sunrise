extends HBoxContainer

@onready var open_dialog: FileDialog = get_node("/root/Main/OpenFileDialog")

signal box_background_color_changed(color: Color)

func _on_font_color_color_changed(color: Color) -> void:
    # Global.cur_font_color = color
    pass

func _on_background_color_color_changed(color: Color) -> void:
    emit_signal("box_background_color_changed", color)

func _on_open_pressed() -> void:
    print("Open file2...")
    open_dialog.popup_centered()

func _on_main_font_size_changed(font_size: Variant) -> void:
    %Size.value = font_size

func _on_main_font_changed(_font_no: Variant, font: Font) -> void:
    %Font.text = font.get_font_name()

func _on_main_font_color_changed(font_color: Variant) -> void:
    %FontColor.color = font_color
