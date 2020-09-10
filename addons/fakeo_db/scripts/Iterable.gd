extends Resource

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")

var source = []
var index = -1
var current
var terminal = false
var query_data = {}
var proc:Proc.Processor

func _init(proc_, source_=[]):
	index = -1
	source = [] + source_ # TODO: copy src?; [] + source
	proc = proc_

# exit by default
func next(item, data):
	return proc.next(item, data)

func make_data():
	return proc.make_data()

func apply(coll):
	var data = make_data()
	var result = []
	for n in coll:
		var r = proc.next(n, data)
		if r[1]:
			result.append(r[0])
		if r[2]: break
	return result

func run() -> Array:
	return apply(source)

func _terminal(coll, idx):
	return terminal or idx > coll.size()-1

func _iter_init(arg):
	if source.empty(): return false
	reset()
	query_data = make_data()
	index = 0
	return _iter_next(arg)
			
func _iter_next(arg):
	if terminal: return false
	var r = []
#	for i in range(idx, source.size()):
	while not terminal:
		r = next(source[index], query_data)
		index += 1
		terminal = r[2]
		if r[1]:
			current = r[0]
			return true
		elif terminal: break
	return false

func _iter_get(arg):
	return current

func reset():
	index = -1
	current = null
	terminal = false
	query_data = {}
	
# get item at index in enumerable
func at(index):
	var next = _iter_init(null)
	while next and index > 0:
		index -= 1
		next = _iter_next(null)
	var r = current
	reset()
	if next:
		return r
	return null
	
func contains(i) -> bool:
	return i in run()
	
func front():
	return at(0)

func ffront():
	return at(1)

func back():
	var result = run()
	if result.empty():
		return null
	return result.back()
	
func bback():
	var result = run()
	var N = result.size()
	if N < 2:
		return null
	return result[N-2]
			
func size() -> int:
	return run().size()

func empty() -> bool:
	return size() == 0
