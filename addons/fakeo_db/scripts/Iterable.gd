extends Resource
# TODO: wtf is query_data? This needs to be obvious somewhere
var source = []
var index = -1
var query:Array = []
var query_data:Array = []
var current
var terminal = false
				
func _init(query_, source_=[]):
	index = -1
	source = [] + source_ # TODO: copy src?; [] + source
	query = query_
	
func run():
	return run_qry_coll(query, get_qry_data(query), source)
		
func _iter_init(arg):
	if source.empty(): return false
	reset()
	query_data = get_qry_data(query)
	index = 0
	return _iter_next(arg)
			
func _iter_next(arg):
	if terminal: return false
	var idx = index
	var r = []
	for i in range(idx, source.size()):
		r = run_qry_item(query, source[i], query_data)
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

# TODO: test
# get item at index in enumerable
func at(index):
	# slice source -> run_qry_coll and return last result
	var next = _iter_init(null)
	while next and index > 0:
		index -= 1
		next = _iter_next(null)
	var r = current
	reset()
	if next:
		return r
	return null
	

# get first in enumerable that satisfies conditions
func front():
	return at(0)

func back():
	var result = run()
	if result.empty():
		return null
	return result.back()
	
func size():
	return run().size()
				
static func run_qry_coll(qry, data, coll):
	var result = []
	for n in coll:
		var r = run_qry_item(qry, n, data)
		if r[1]:
			result.append(r[0])
		if r[2]: break
	return result
	
static func run_qry_item(qry, item, data):
	var r = [item, true, false]
	for i in qry.size():
		var q = qry[i]
		r = q.next(r[0], data[i])
		if not r[1] or r[2]: break
	return r

static func get_qry_data(qry):
	var r = []
	for q in qry:
		r.append(q.make_data())
	return r
				
