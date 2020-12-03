extends Node

const Collection = preload("res://addons/fakeo_db/scripts/Enumerables.gd").Collection

const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Transducers = preload("res://addons/fakeo_db/scripts/Transducers.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

const OpFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")
const TdxFactory = preload("res://addons/fakeo_db/scripts/TransducerFactory.gd")

const TdxChainer = preload("res://addons/fakeo_db/scripts/TransducerChainer.gd")

const HeapSort = preload("res://addons/fakeo_db/scripts/Algo/heap_sort.gd")

static func cltn(array:Array=[]) -> Collection:
	return Collection.new(array)

# 'tdxs' should be Array(Transducer)
static func chain(tdxs:Array=[]) -> TdxChainer:
	return TdxChainer.new(tdxs)
	

static func itbl(input, coll) -> Iterable:	
	if input is Array:
		input = Transducers.TdxIterator.new(input)
	assert(input is Transducers.Transducer)
	return Iterable.new(input, coll)


static func apply(input, coll) -> Array:
	return itbl(input, coll).run()

static func reduce(op: Operators.OperatorBase, coll):
	return chain().ittr(op, coll).back()

static func sort(op: Operators.OperatorBase, coll) -> Array:
	return HeapSort.heap_sort(op, coll)

	
static func first(op: Operators.OperatorBase, coll):
	return chain().filter(op, coll, true).front()

static func last(op: Operators.OperatorBase, coll):
	return chain().filter(op, coll, true).back()
