tool
extends EditorPlugin

const LIB_NAME = 'EasyQueries'
const SCRIPTS_PATH = 'res://addons/easy_queries/scripts/'

func _enter_tree():
	self.add_autoload_singleton(LIB_NAME, SCRIPTS_PATH+LIB_NAME+".gd")

func _exit_tree():
	self.remove_autoload_singleton(LIB_NAME)