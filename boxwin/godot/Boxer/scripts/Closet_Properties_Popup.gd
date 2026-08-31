extends PopupMenu

func _on_id_pressed(id: int) -> void:
    match id:
        0:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-FULL-SCREEN-BOX")
        1:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-NORMAL-SIZE-BOX")
        2:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-SHRINK-BOX")
        3:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-SUPERSHRINK-BOX")
        4: # Separator
            pass
        5:
            $/root/Main.handle_boxer_func("COM-HOTSPOT-TOGGLE-CLOSET")
        6:
            $/root/Main.handle_boxer_func("COM-EDIT-BOX-PROPERTIES")



