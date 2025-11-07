extends Node2D


func _on_turtle_done_drawing() -> void:
    print("done drawing turtle 43")
    var img = $TurtleViewportSprite.get_texture().get_image()
    var tex = ImageTexture.create_from_image(img)
    $FinalSprite.texture = tex
    $SubViewportContainer/SubViewport/turtle.queue_redraw()
