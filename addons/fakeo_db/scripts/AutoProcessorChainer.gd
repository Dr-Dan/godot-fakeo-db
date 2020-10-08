extends "Processors.gd".ProcIterator

const Itbl = preload("res://addons/fakeo_db/scripts/Iterable.gd")
const prc = preload("res://addons/fakeo_db/scripts/ProcessorFactory.gd")
const Procs = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase
const HeapSort = preload("res://addons/fakeo_db/scripts/Algo/heap_sort.gd")

# -------------------------------------------------------

func _init(procs_:Array=[]).(procs_) -> void:
	pass

# -------------------------------------------------------

func get_proc(input, coll=null, itbl=false):
	if coll == null:
		return append(input)
	assert(input is Procs.Processor)
	var c = Itbl.new(append(input), coll)
	if itbl:
		return c
	return c.apply(coll)

# func then(data):
# 	if data is Array:
# 		return append_array(data)
# 	assert(data is Procs.Processor)
# 	return append(data)

# override these two if extending
func append(item):
	return get_script().new(procs + [item])
	
func append_array(items:Array):
	return get_script().new(procs + items)
	
# -------------------------------------------------------
# instant eval

func itbl(coll=[]):
	return Itbl.new(self, coll)
	
func apply(coll):
	return itbl(coll).run()
	
	
func reduce(op, coll):
	return ittr(Operators.Util.get_map_op(op), coll).back()

func sort(op, coll):
	return HeapSort.heap_sort(op, apply(coll))

	
func first(op, coll):
	return filter(op, coll).front()

func last(op, coll):
	return filter(op, coll).back()

# -------------------------------------------------------
# chain

func comp(input:Array, coll=null, itbl=false):
	return get_proc(prc.comp(input), coll, itbl)

func map(input, coll=null, itbl=false):
	return get_proc(prc.map(Operators.Util.get_map_op(input)), coll, itbl)

func filter(input, coll=null, itbl=false):
	return get_proc(prc.filter(Operators.Util.get_filter_op(input)), coll, itbl)

func project(input, coll=null, itbl=false):
	return get_proc(prc.project(input), coll, itbl)
	
func take(amt: int, coll=null, itbl=false):
	return get_proc(prc.take(amt), coll, itbl)

func take_while(op: OpBase, coll=null, itbl=false):
	return get_proc(prc.take_while(op), coll, itbl)

func skip(amt: int, coll=null, itbl=false):
	return get_proc(prc.skip(amt), coll, itbl)

func ittr(op, coll=null, itbl=false):
	return get_proc(prc.ittr(op), coll, itbl)

func as_args(fn:FuncRef, coll=null, itbl=false):
	return get_proc(prc.map(Operators.FuncAsArgs.new(fn)), coll, itbl)
