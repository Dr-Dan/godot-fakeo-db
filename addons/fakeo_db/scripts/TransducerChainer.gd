extends "Transducers.gd".TdxIterator
class_name TdxChainer

const Itbl = preload("res://addons/fakeo_db/scripts/Iterable.gd")
const TdxFac = preload("res://addons/fakeo_db/scripts/TransducerFactory.gd")
const Tdxs = preload("res://addons/fakeo_db/scripts/Transducers.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase
const HeapSort = preload("res://addons/fakeo_db/scripts/Algo/heap_sort.gd")

# -------------------------------------------------------

func _init(procs_:Array=[]).(procs_):
	pass

# -------------------------------------------------------

func get_result(input:Tdxs.Transducer, coll=null, itbl:bool=false):
	if coll == null:
		return append(input)
	var c = Itbl.new(append(input), coll)
	if itbl:
		return c
	return c.apply(coll)

func then(data):
	if data is Array:
		return append_array(data)
	assert(data is Tdxs.Transducer)
	return append(data)

# override these two if extending
func append(item:Tdxs.Transducer):
	return get_script().new(procs + [item])
	
func append_array(items:Array):
	return get_script().new(procs + items)
	
# -------------------------------------------------------
# instant eval i.e. no chaining

func itbl(coll=[]):
	return Itbl.new(self, coll)
	
func apply(coll):
	return itbl(coll).run()
	
	
func reduce(op, coll):
	return ittr(Operators.Util.get_map_op(op), coll).back()

func sort(op, coll):
	return HeapSort.heap_sort(Operators.Util.get_map_op(op), apply(coll))

	
func first(op, coll):
	return filter(op, coll).front()

func last(op, coll):
	return filter(op, coll).back()

# -------------------------------------------------------
# chainers

func comp(input:Array, coll=null, itbl=false):
	return get_result(TdxFac.comp(input), coll, itbl)

func map(input, coll=null, itbl=false):
	return get_result(TdxFac.map(Operators.Util.get_map_op(input)), coll, itbl)

func filter(input, coll=null, itbl=false):
	return get_result(TdxFac.filter(Operators.Util.get_filter_op(input)), coll, itbl)

func project(input, coll=null, itbl=false):
	return get_result(TdxFac.project(input), coll, itbl)
	
func take(amt: int, coll=null, itbl=false):
	return get_result(TdxFac.take(amt), coll, itbl)

func take_while(op: OpBase, coll=null, itbl=false):
	return get_result(TdxFac.take_while(op), coll, itbl)

func skip(amt: int, coll=null, itbl=false):
	return get_result(TdxFac.skip(amt), coll, itbl)

func ittr(op, coll=null, itbl=false):
	return get_result(TdxFac.ittr(op), coll, itbl)

func as_args(fn:FuncRef, coll=null, itbl=false):
	return get_result(TdxFac.map(Operators.FuncAsArgs.new(fn)), coll, itbl)
