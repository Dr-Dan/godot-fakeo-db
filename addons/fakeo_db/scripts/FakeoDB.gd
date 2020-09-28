extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Processors = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")
const ProcessorFactory = preload("res://addons/fakeo_db/scripts/ProcessorFactory.gd")

const AutoProcessorChainer = preload("res://addons/fakeo_db/scripts/AutoProcessorChainer.gd")

const HeapSort = preload("res://addons/fakeo_db/scripts/Algo/heap_sort.gd")

static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)


# procs should be Array(Processor)
static func flex(procs=[]):
	return AutoProcessorChainer.new(procs)
	

static func itbl(input, coll) -> Iterable:	
	if input is Array:
		input = Processors.ProcIterator.new(input)
	assert(input is Processors.Processor)
	return Iterable.new(input, coll)


static func apply(input, coll) -> Array:
	return itbl(input, coll).run()

static func reduce(op, coll):
	return flex().ittr(op, coll).back()

static func sort(op, coll):
	return HeapSort.heap_sort(op, coll)

	
static func first(op, coll):
	return flex().filter(op, coll, true).front()

static func last(op, coll):
	return flex().filter(op, coll, true).back()
