class_name QueryBuilderDeferred

var start
var next

# TODO: could just set eval(e:Enuerable) input as start
func _init():
	start = EnumeratorsDeferred.ListEnumerator.new([])
	next = start
	
#func get_start(e):
#	if start == null:
#		return e
#	else:
#		return start
		
func project(fields):
	next = EnumeratorsDeferred.Project.new(next, fields)
	return self

func where(comps):
	next = EnumeratorsDeferred.Where.new(next, comps)
	return self

func take(amt):
	next = EnumeratorsDeferred.Take.new(next, amt)
	return self

func eval(data):
	start._init(data)
	return next