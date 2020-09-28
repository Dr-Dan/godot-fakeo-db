extends Resource

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")

var source = []
var index = -1
var current
var terminal = false
var query_data = {}
var proc:Proc.Processor

func _init(proc_=null, source_=[]):
	index = -1
	source = source_
	proc = proc_

func next(item, data):
	return proc.next(item, data)

func apply(coll) -> Array:
	var data = proc.make_data()
	var result = []
	for n in coll:
		var r = proc.next(n, data)
		if r is Proc.Terminate: break
		if not r is Proc.None:
			result.append(r)
	return result

func run() -> Array:
	return apply(source)

func _iter_init(arg):
	if source.empty(): return false
	reset()
	query_data = proc.make_data()
	index = 0
	return _iter_next(arg)
			
func _iter_next(arg):
	if terminal: return false
	var r = []
	var st = index
	for i in range(st, source.size()):
		r = next(source[index], query_data)
		index += 1
		if r is Proc.Terminate: break
		if not r is Proc.None:
			current = r
			return true
		
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
