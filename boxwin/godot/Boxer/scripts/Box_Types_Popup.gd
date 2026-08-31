extends PopupMenu


func _on_id_pressed(id: int) -> void:
    match id:
        0:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-TOGGLE-GRAPHICS")
        1:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-TOGGLE-GRAPHICS")
        2: # Separator
            pass
        3:
            $/root/Main.handle_boxer_func("COM-CHANGE-BOX-TO-DATA")
        4:
            $/root/Main.handle_boxer_func("COM-CHANGE-BOX-TO-DOIT")
