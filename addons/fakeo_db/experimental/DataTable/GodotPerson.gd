extends Sprite
	

var wpn_name = "Godot Person"
var type = "summon"
var subtype = "sprite"
var dmg = 666
var atk_rate = 1.0
var atk_range = 100.0

var x setget _set_x, _get_x
var y setget _set_y, _get_y
#var y

func _get_x():
	return position.x
	
func _set_x(val):
	position.x	= val

func _get_y():
	return position.y
	
func _set_y(val):
	position.y	= val
