extends Node2D


func _on_start_pressed() -> void:
    %Video.play()

func _process(_delta: float) -> void:
    %Position.text = str(%Video.stream_position)

func _on_stop_pressed() -> void:
    %Video.stop()


func _on_seek_pressed() -> void:
    %Video.stream_position = 8.0


func _on_pause_pressed() -> void:
    %Video.paused = true
