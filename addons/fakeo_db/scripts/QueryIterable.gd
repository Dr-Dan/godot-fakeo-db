extends "Query.gd"
#class_name Query

var _iter:Iterable = null
var source = []

func _init(source_=[], items_:Array=[]).(items_) -> void:
	source = source_
	_iter = Iterable.new(items_, source)

# these use different init from base so must be overwritten
func proc(item):
	return get_script().new(source, items + [item])
	
func proc_array(item:Array):
	return get_script().new(source, items + item)
	
func _iter_init(arg):
	_iter = Iterable.new(items, source)
	return _iter._iter_init(arg)
			
func _iter_next(arg):
	return _iter._iter_next(arg)

func _iter_get(arg):
	return _iter._iter_get(arg)

func run():
	return _iter.run()

func at(idx):
	return _iter.at(idx)

func front():
	return _iter.front()

func back():
	return _iter.back()

func size():
	return _iter.size()