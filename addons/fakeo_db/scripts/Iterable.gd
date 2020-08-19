extends Resource

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")

var source = []
var index = -1
var proc:Proc.ProcIterator
var current
var terminal = false
var query_data = {}	

func _init(query_, source_=[]):
	index = -1
	source = [] + source_ # TODO: copy src?; [] + source
	proc = Proc.ProcIterator.new(query_)
	
func run() -> Array:
	var result = []
	var data = proc.make_data()
	if proc.procs.size() == 1:
		return proc.apply(source, data)

	for n in source:
		var r = proc.next(n, data)
		if r[1]:
			result.append(r[0])
		if r[2]: break
	return result	

func _iter_init(arg):
	if source.empty(): return false
	reset()
	query_data = proc.make_data()
	index = 0
	return _iter_next(arg)
			
func _iter_next(arg):
	if terminal: return false
	var idx = index
	var r = []
	for i in range(idx, source.size()):
		r = proc.next(source[i], query_data)
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
