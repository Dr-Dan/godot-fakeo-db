extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Processors = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")
const ProcessorFactory = preload("res://addons/fakeo_db/scripts/ProcessorFactory.gd")
const AutoProcessors = preload("res://addons/fakeo_db/scripts/AutoProcessors.gd")

static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)
	

static func itbl(input, coll) -> Iterable:
	return Iterable.new(_input_to_proc(input), coll)

static func mitbl(op, coll) -> Iterable:
	return itbl(Processors.MapOp.new(_input_to_op(op)), coll)

static func fitbl(op, coll) -> Iterable:
	return itbl(Processors.FilterOp.new(_input_to_op(op)), coll)


static func apply(input, coll) -> Array:
	return itbl(input, coll).run()
	
static func mapply(input, coll) -> Array:
	return mitbl(input, coll).run()
	
static func fapply(input, coll) -> Array:
	return fitbl(input, coll).run()
	

static func ittr(op, coll):
	return itbl(Processors.IterateOp.new(op), coll)

static func reduce(op, coll):
	return ittr(op, coll).back()

	
static func _input_to_op(input):
	if input is Array:
		input = Operators.OperatorIterator.new(input)
	assert(input is Operators.OperatorBase)
	return input

static func _input_to_proc(input) -> Processors.Processor:
	if input is Iterable:
		return input.proc
	elif input is Array:
		return Processors.ProcIterator.new(input)
	assert(input is Processors.Processor)
	return input

			
# static func _qry_to_arr(query) -> Array:
# 	if query is Iterable:
# 		return query.proc.procs
# 	elif query is Processors.Processor:
# 		return [query]
# 	assert(query is Array)
# 	return query
