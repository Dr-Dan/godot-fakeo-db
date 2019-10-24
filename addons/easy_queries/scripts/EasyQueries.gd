extends Node

const SCRIPTS_PATH = 'res://addons/easy_queries/scripts/'

const ed = SCRIPTS_PATH + "EnumeratorsDeferred.gd"
const qbd = SCRIPTS_PATH + "QueryBuilderDeferred.gd"

const ops = SCRIPTS_PATH + "Operators.gd"
const of = SCRIPTS_PATH + "OperatorFactory.gd"

const Enumerators = preload(ed)
const QueryBuilder = preload(qbd)

const Operators = preload(ops)
const OperatorFactory = preload(of)