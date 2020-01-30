extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const QueryBuilder = preload("res://addons/fakeo_db/scripts/QueryBuilder.gd")

const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")

static func list(array:Array=[]):
	return Enumerables.List.new(array)
	
static func cltn(array:Array=[]):
	return Enumerables.Collection.new(array)
	
static func qb():
	return QueryBuilder.new()