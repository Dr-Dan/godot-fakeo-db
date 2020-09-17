extends Resource

const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")
const Procs = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Processor = Procs.Processor
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase

static func get_proc(input, coll=null, itbl=false):
	if coll == null:
		return input
	assert(input is Processor)
	var c = Iterable.new(input, coll)
	if itbl:
		return c
	return c.apply(coll)

# -------------------------------------------------------
static func comp(input:Array, coll=null, itbl=false):
	return get_proc(Procs.ProcIterator.new(input), coll, itbl)

static func map(input, coll=null, itbl=false):
	return get_proc(Procs.MapOp.new(input), coll, itbl)

static func filter(input, coll=null, itbl=false):
	return get_proc(Procs.FilterOp.new(input), coll, itbl)

static func take(amt: int, coll=null, itbl=false):
	return get_proc(Procs.Take.new(amt), coll, itbl)

static func take_while(op: OpBase, coll=null, itbl=false):
	return get_proc(Procs.TakeWhile.new(op), coll, itbl)

static func skip(amt: int, coll=null, itbl=false):
	return get_proc(Procs.Skip.new(amt), coll, itbl)

static func ittr(op, coll=null, itbl=false):
	return get_proc(Procs.IterateOp.new(op), coll, itbl)

static func enumerate(key='i', step=1, coll=null, itbl=false):
	return get_proc(Procs.Enumerate.new(key, step), coll, itbl)

static func as_args(fn:FuncRef, coll=null, itbl=false):
	return get_proc(map(Operators.FuncAsArgs.new(fn)), coll, itbl)

	
