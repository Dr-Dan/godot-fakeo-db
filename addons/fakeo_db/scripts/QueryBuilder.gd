const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")

var start
var next

func _init():
	pass
	
func _set_next(e):
	if start == null:
		start = e
	next = e
		
func project(fields:Array):
	_set_next(Enumerables.Project.new(next, fields))
	return self

func where(comps):
	var where_cls = Enumerables.Enumerable.get_where_type(next, comps)
	_set_next(where_cls)
	return self

func take(amt):
	_set_next(Enumerables.Take.new(next, amt))
	return self

func select(select_func: FuncRef, arg=null):
	_set_next(Enumerables.Select.new(next, select_func, arg))
	return self
	
func select_op(op):
	_set_next(Enumerables.SelectOp.new(next, op))
	return self

# expects Array or child of Enumerable
func eval(data):
	if data is Array:
		data = Enumerables.List.new(data)
	# assert(data is Enumerables.Enumerable)
	start.source = data
	return next
