extends "res://addons/fakeo_db/scripts/Enumerables.gd".List

signal on_item_added(item)
signal on_item_erased(item)
signal on_cleared

func _init(source:Array=[]).(source):
	pass
	
func append(item):
	source.append(item)
	emit_signal("on_item_added", item)
			
func erase(item):
	if item in source:
		source.erase(item)
		emit_signal("on_item_erased", item)
		
func erase_at(index):
	assert(index>=0)
	if index < source.size():
		var item = source[index]
		erase(item)	
		
func empty():
	return source.empty()

func clear():
	emit_signal("on_cleared")
	source.clear()
	
func size():
	return source.size()