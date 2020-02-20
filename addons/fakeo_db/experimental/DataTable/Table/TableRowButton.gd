extends Control
class_name TableRowButton

onready var button = $Button
onready var data = $Data

func get_cell(index):
	return data.get_cell(index)

func set_cell_value(index, value):
	data.set_cell_value(index, value)
		
func add_cell(c):
	data.add_child(c)
	
func get_cell_count():
	return data.get_child_count()
