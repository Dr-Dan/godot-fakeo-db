extends MarginContainer

var color setget _set_color
var value setget _set_text

onready var label = $Label
onready var color_rect = $ColorRect

func _set_color(col):
	color_rect.color = col	
	color = col
	
func _set_text(val):
	label.text = val	
	value = val
