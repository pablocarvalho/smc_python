import ply.lex as lex
import ply.yacc as yacc
import os
import numbers
import node as nd
from enum import Enum


class SMCmachine:
	def __init__(self):         
         self.s = Stack()
         self.m = []
         self.c = Stack()
         self.e = Stack()
         self.debug = True

	def print_state(self):
		if(self.debug == True):
			state = '<'+ str(self.e.items) + 'E,  '+ str(self.s.items) + 'S,  ' + str(self.m) + 'M,  ' + str(self.c.items) + 'C>'
			print state

	def load_and_run(self, current_node):
		if(isinstance(current_node,nd.Node)):
			if(current_node.type == 'binop'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[1])
				self.load_and_run(current_node.children[0]) 					 	
				self.c.pop()
				self.evaluate_binary_op(current_node.leaf[0])
				self.print_state()

			elif(current_node.type == 'separator'):
				self.load_and_run(current_node.children[0])
				self.load_and_run(current_node.children[1])
				self.print_state()

			elif(current_node.type == 'assign' or current_node.type == 'boolassign'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0])				
				self.load_and_run(current_node.children[1])


				self.c.pop()
				value = self.s.pop()
				variable = self.s.pop()
				enviromentPos = self.enviromentSearch(variable)

				if(enviromentPos == -1):
					raise Exception("variable '"+variable+"' not found")
				elif(isinstance(self.e.items[enviromentPos],EnviromentConst)):
					raise Exception(variable +" is a const")
				else:
					memAddress = self.e.items[enviromentPos]
					memCell = self.m[memAddress.address]
					if( type(memCell.data) != type(value) ):
						raise Exception("trying to store "+ type(value).__name__ +" value in " + type(memCell.data).__name__+ " variable" )
					else:
						memCell.data = value
				
				
				self.print_state()

			elif(current_node.type == 'booleanexpression_binop'):
				self.c.push(current_node)
				self.load_and_run(current_node.children[0]) 	
				self.load_and_run(current_node.children[1]) 	
				self.c.pop()				
				self.evaluate_binary_bool(current_node.leaf)
				self.print_state()

			elif(current_node.type == 'booleanexpressionnot'):
				self.c.push(current_node)
				child0 = self.load_and_run(current_node.children[0])				
				value = not self.s.pop()
				self.s.push(value)
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'conditional' or current_node.type == 'expression_conditional'):

				self.c.push(current_node)

				self.load_and_run(current_node.children[0])

				booleanblockResult = self.s.pop()
				self.print_state()

				
				if(booleanblockResult == True):
					self.load_and_run(current_node.children[1])
				else:
					self.load_and_run(current_node.children[2])	
				
				self.c.pop()
				self.print_state()

			elif(current_node.type == 'loop'):

				self.c.push(current_node)

				self.load_and_run(current_node.children[0])
				booleanblockResult = self.s.pop()
				self.print_state()

				while(booleanblockResult == True):
					self.load_and_run(current_node.children[1])

					self.load_and_run(current_node.children[0])
					booleanblockResult = self.s.pop()
					self.print_state()

				self.c.pop()
				self.print_state()

			elif(current_node.type == 'command_block'):
				self.c.push(current_node)				

				actualEnviroment = list(self.e.items)

				self.load_and_run(current_node.children[0])

				self.c.pop()
				self.e.items = actualEnviroment
				self.print_state()
				self.freeMemory()
				self.print_state()

			elif(current_node.type == 'booleanexpression_block'):
				self.load_and_run(current_node.children[0])
				

			elif(current_node.type == 'declaration_const'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[2])								
				value = self.s.pop() #get a value from values stack
				constName = current_node.children[0]
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeConstOnEnviroment(EnviromentConst(constName,value))					
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.c.pop()
				self.print_state()		

			elif(current_node.type == 'declaration_var'):
				self.c.push(current_node)				
				self.load_and_run(current_node.children[2])	
				value = self.s.pop() #get a value from values stack							
				varName = current_node.children[0]
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeVarOnMemory(varName,value)
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.c.pop()
				self.print_state()
				
		elif(current_node == 'true' or current_node == 'false'):
			value = (current_node == 'true')
			self.s.push(value)
			self.print_state()

		else:
			self.s.push(current_node)
			self.print_state()


	def load_program(self, current_node):

		if(isinstance(current_node,nd.Node)):
			if(current_node.type == 'binop'):
				self.c.push(current_node.leaf[0])
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()

			elif(current_node.type == 'separator'):
				
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.c.push(current_node.leaf[0])

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)

				self.print_state()

			elif(current_node.type == 'assign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()

			elif(current_node.type == 'booleanexpression_binop'):
				self.c.push(str(current_node.leaf))
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()
			
			elif(current_node.type == 'boolassign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()
			elif(current_node.type == 'booleanvalues'):
				self.c.push(current_node.leaf)
				
				self.print_state()
			
			elif(current_node.type == 'booleanexpressionnot'):
				self.c.push(str(current_node.leaf))
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				
				
				self.print_state()

			elif(current_node.type == 'loop'):

				
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)

				self.c.push(current_node.leaf[1])	
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(current_node.leaf[0])	
				self.print_state()


			elif(current_node.type == 'conditional' or current_node.type == 'expression_conditional'):

				if(current_node.type == 'expression_conditional'):
					self.c.push(current_node.leaf[3])

				child2 = self.load_program(current_node.children[2])
				if(child2 is not None):			
					self.c.push(child2)
				self.c.push(current_node.leaf[2])

				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)

				self.c.push(current_node.leaf[1])	
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(current_node.leaf[0])	
				self.print_state()			

			elif(current_node.type == 'command_block' or current_node.type == 'booleanexpression_block'):
				blockContent = Stack()				
				self.c.push(current_node.leaf[0])
				first_par_pos = self.c.size() - 1
				child0 = self.load_program(current_node.children[0]) 										
				self.c.push(current_node.leaf[1])				
				last_par_pos = self.c.size() - 1
				self.print_state()

				block_elements_list = self.c.items[ (first_par_pos+1) : (last_par_pos) ]   
				blockContent.items = block_elements_list
				

				del self.c.items[first_par_pos:last_par_pos+1]

				if(current_node.type == 'command_block'):
					commandBlock = Command(blockContent)
					self.c.push(commandBlock)
				else:
					commandBlock = BooleanBlock(blockContent)
					self.c.push(commandBlock)

							

				self.print_state()

			elif(current_node.type == 'declaration_var'):
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf[0]))
				child1 = self.load_program(current_node.children[2])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()

			elif(current_node.type == 'declaration_const'):
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf[0]))
				child1 = self.load_program(current_node.children[2])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()

		else:
			return current_node		


	def run_program(self):

		print "\n running program \n"
		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		
		self.print_state();


	def eval_control(self, control):
		if(isinstance(control,numbers.Number)):
				self.s.push(control)
				self.print_state();

		elif(isinstance(control,str)):
			if(control == 'add' or control == 'div' or control == 'mul' or control == 'sub'):
				self.evaluate_binary_op(control)	
				
			elif(control == 'and' or control == 'or' or control == 'eq'):
				self.evaluate_binary_bool(control)	
				
			elif(control=='true' or control=='false'):
				value = self.s.push(control == 'true');					
				self.print_state();

			elif(control =='not'):
				value = self.s.pop();
				self.s.push(not value)

			elif(control=='while'):
				self.print_state();
				booleanblock = self.c.pop()

				
				boolstack = Stack()
				boolstack.items = list(booleanblock.content.items)
				booleanblock_copy = BooleanBlock(boolstack)
				self.eval_boolean_block(booleanblock_copy)

				self.print_state();
				eval_result = self.s.pop()
				do_word = self.c.pop()
				self.print_state();

				commandblock = self.c.pop()

				cmdstack = Stack()
				cmdstack.items = list(commandblock.command.items)
				commandblock_copy = Command(cmdstack)

				if(eval_result):
					self.eval_command_block(commandblock_copy)

					self.c.push(commandblock)	
					self.c.push(do_word)
					self.c.push(booleanblock)
					self.c.push(control)
					self.print_state()

			elif(control == 'if'):
				booleanblock = self.c.pop()
				self.print_state()
				self.eval_boolean_block(booleanblock)

				self.print_state()
				eval_result = self.s.pop()
				self.print_state()
				word_then = self.c.pop()
				self.print_state()

				nextControlElem = self.c.pop()

				if(isinstance(nextControlElem,Command)):
					commandblock = nextControlElem				
					self.print_state()
					word_else = self.c.pop()
					self.print_state()
					elsecommandblock = self.c.pop()
					self.print_state()

					if(eval_result):
						self.eval_command_block(commandblock)
					else:
						self.eval_command_block(elsecommandblock)
				else:
					if(eval_result):
						auxiliarStack = Stack()
						while(nextControlElem != 'else'):
							auxiliarStack.push(nextControlElem)
							nextControlElem = self.c.pop()

						while(nextControlElem != 'endif'):
							nextControlElem = self.c.pop()

						while(not auxiliarStack.isEmpty()):
							self.c.push(auxiliarStack.pop())

					else:
						auxiliarStack = Stack()
						while(nextControlElem != 'else'):
							nextControlElem = self.c.pop()

						nextControlElem = self.c.pop()
						while(nextControlElem != 'endif'):
							auxiliarStack.push(nextControlElem)
							nextControlElem = self.c.pop()

						while(not auxiliarStack.isEmpty()):
							self.c.push(auxiliarStack.pop())

					
				self.print_state()

					
			elif(control == ':='):
				variable = self.c.pop();
				value = self.s.pop();
				enviromentPos = self.enviromentSearch(variable)

				if(enviromentPos == -1):
					raise Exception("variable '"+variable+"' not found")
				elif(isinstance(self.e.items[enviromentPos],EnviromentConst)):
					raise Exception(variable +" is a const")
				else:
					memAddress = self.e.items[enviromentPos]
					memCell = self.m[memAddress.address]
					if( type(memCell.data) != type(value) ):
						raise Exception("trying to store "+ type(value).__name__ +" value in " + type(memCell.data).__name__+ " variable" )
					else:
						memCell.data = value
					
			elif(control == ';'):
				self.print_state()

			elif(control == 'const'):				
				constName = self.c.pop()
				value = self.s.pop() #get a value from values stack
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeConstOnEnviroment(EnviromentConst(constName,value))					
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.print_state()

			elif(control == 'var'):				
				varName = self.c.pop()
				value = self.s.pop() #get a value from values stack
				if(isinstance(value,numbers.Number) or isinstance(value,bool)):
					self.storeVarOnMemory(varName,value)
				else:
					raise Exception("Unknown type '"+type(value).__name__+"' when declaring constant")

				self.print_state()

			else:
				self.s.push(control)
				self.print_state();
	
	def eval_command_block(self, commandblock):

		actualControlStack = list(self.c.items)
		self.c.items = commandblock.command.items
		#print "command_block" + str(actualControlStack)

		actualEnviroment = list(self.e.items)

		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		self.c.items = actualControlStack
		self.e.items = actualEnviroment



	def eval_boolean_block(self, booleanblock):
		self.debug = False

		actualControlStack = list(self.c.items)
		self.c.items = booleanblock.content.items
		#print "boolean_block" + str(actualControlStack)

		while(not self.c.isEmpty() ):

			control = self.c.pop()
			self.eval_control(control)

		self.c.items = actualControlStack

		self.debug = True

	def enviromentSearch(self,varName):
		index = len(self.e.items) - 1		
		for item in reversed(self.e.items):			
			if(isinstance(item,EnviromentConst)):
				if(item.name == varName):
					return index
			elif(isinstance(item,MemoryAddress)):
				memoryCell = self.m[item.address]
				if(memoryCell.variableName == varName):
					return index
			index-=1

		return -1

	def evaluate_binary_op(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()				
		if(isinstance(op1,str)):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();		
		if(isinstance(op2,str)):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();

				
		self.print_state()
		if(operator == 'add'):
			self.s.push(op1+op2);
		if(operator == 'sub'):
			self.s.push(op1-op2);
		if(operator == 'mul'):
			self.s.push(op1*op2);
		if(operator == 'div'):
			self.s.push(op1/op2);
		self.print_state();

	def evaluate_binary_bool(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()

		if(not isinstance(op1,bool) and not isinstance(op1,numbers.Number)):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();
		if(not isinstance(op2,bool) and not isinstance(op2,numbers.Number)):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();		

		if(operator == "eq"):
			if(isinstance(op1,numbers.Number) and isinstance(op2,numbers.Number)):
				self.s.push(op1 == op2)			
			else:
				raise Exception("impossible to compare values "+ str(op1) + " and " + str(op2))

		else:			
			if(operator == 'and'):
				self.s.push(op1 and op2)
			if(operator == 'or'):
				self.s.push(op1 or op2)				
		
		self.print_state();

	def loadFromMemoryToS(self,varName):
		address = self.enviromentSearch(varName)

		if(address == -1):
			raise Exception("var '" +varName +"' not found")
		else:
			enviromentItem = self.e.items[address]
			if(isinstance(enviromentItem,EnviromentConst)):
				data = enviromentItem.value
				self.s.push(data)
			elif(isinstance(enviromentItem,MemoryAddress)):
				memoryCell = self.m[enviromentItem.address]
				self.s.push(memoryCell.data)
			self.print_state()

	def storeConstOnEnviroment(self,enviromentConst):
		self.e.push(enviromentConst)

	def storeVarOnMemory(self,varName,varValue):
		
		self.m.append(MemoryCell(varValue,varName))
		self.e.push(MemoryAddress(type(varValue),len(self.m) - 1))			
				

	def loadFromEnviroment(self, varName):

		for envItem in reversed(self.e.items):
			if(isinstance(envItem,EnviromentConst) and envItem.name == varName):
				self.s.push(envItem.value)
			elif(isinstance(envItem,MemoryAddress)):
				memoryCell = self.m[envItem.address]
				if(memoryCell.variableName == varName):
					self.s.push(memoryCell.data)			

	def freeMemory(self):
		itemFound = False

		for idx in range(len(self.m)):
			memItem = self.m[idx]

			itemFound = False
			for envItem in self.e.items:				
				if(isinstance(envItem,MemoryAddress)):
					memoryCell = self.m[envItem.address]
					if(memItem == memoryCell):						
						itemFound = True
						break

			if(itemFound == False):				
				self.m[idx] = None

		self.m = filter(None,self.m)


class Stack:
     def __init__(self):
         self.items = []

     def isEmpty(self):
         return self.items == []

     def push(self, item):
         self.items.append(item)

     def pop(self):
         return self.items.pop()

     def peek(self):
         return self.items[len(self.items)-1]

     def size(self):
         return len(self.items)

class MemoryCell:
	def __init__(self,data, var):
		self.data = data
		self.variableName = var
	def __repr__(self):
		return "M["+str(self.data)+"/"+str(self.variableName)+"]"


class MemoryAddress:
	def __init__(self, varType, memoryReference):
		self.type = varType	
		self.address = memoryReference	

	def __repr__(self):
		return "MemAddr("+self.type.__name__+"|"+str(self.address)+")"

class EnviromentConst:
	def __init__(self, name,value):
		self.name = name
		self.value = value	
		self.type = type(value).__name__

	def __repr__(self):
		return "EnvConst("+self.type+" "+self.name+"="+str(self.value)+")"

class Command:
	def __init__(self, commandlist):
		self.command = commandlist
	def __repr__(self):
		return "c("+str(self.command.items)+")"

class BooleanBlock:
	def __init__(self, booleanBlockcontent):
		self.content = booleanBlockcontent
	def __repr__(self):
		return "c("+str(self.content.items)+")"

class Value:
	def __init__(self, commandstring):
		self.command = comandstring
	def __repr__(self):
		return "v("+str(self.command)+")"


