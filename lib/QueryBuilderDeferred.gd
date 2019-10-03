class_name QueryBuilderDeferred

var start
var next

func _init():
	pass
	
func _set_next(e: EnumeratorsDeferred.Enumerable):
	if start == null:
		start = e
	next = e
		
func project(fields):
	_set_next(EnumeratorsDeferred.Project.new(next, fields))
	return self

func where(comps):
	_set_next(EnumeratorsDeferred.Where.new(next, comps))
	return self

func take(amt):
	_set_next(EnumeratorsDeferred.Take.new(next, amt))
	return self

func eval(data):
	if data is Array:
		data = EnumeratorsDeferred.ListEnumerator.new(data)
	start.source = data
	return next