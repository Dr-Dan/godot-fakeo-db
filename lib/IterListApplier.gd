#extends Resource
class_name IterListApplier

var start
var curr
var end
var increment
var items = []

func _init(items:Array=[]):
	increment = 1
	start = 0
	curr = start
	self.items = items
	self.end = items.size()
	
func size():
	return items.size()

# NOTE: this may be overridden for child classes
func should_continue():
	return (curr < end)

func _iter_init(arg):
	curr = start
	return should_continue()

func _iter_next(arg):
	curr += increment
	return should_continue()

# TODO: return next evaluated item
func _iter_get(arg):
	return items[curr]
	
func apply(query):
	var d = items
	for i in query:
		d = i.eval(d)
	if d is Array:
		d = get_script().new(d)
	return d