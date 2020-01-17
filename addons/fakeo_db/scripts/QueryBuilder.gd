const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")

var start
var next

func _init():
	pass
	
func _set_next(e):
	if start == null:
		start = e
	next = e
		
func project(fields):
	_set_next(Enumerables.Project.new(next, fields))
	return self

func where(comps):
	var where_cls = Enumerables.Enumerable.get_where_type(comps)
	_set_next(where_cls.new(next, comps))
	return self

func take(amt):
	_set_next(Enumerables.Take.new(next, amt))
	return self

func select(select_func: FuncRef, arg=null):
	_set_next(Enumerables.Select.new(next, select_func, arg))
	return self
	
func eval(data):
	if data is Array:
		data = Enumerables.List.new(data)
	start.source = data
	return next