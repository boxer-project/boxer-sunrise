extends Control

func _ready() -> void:
    $ScrollContainer.get_v_scrollbar().rect_min_size.x = 50
