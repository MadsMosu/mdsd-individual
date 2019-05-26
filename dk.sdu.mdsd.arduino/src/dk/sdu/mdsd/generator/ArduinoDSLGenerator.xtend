package dk.sdu.mdsd.generator

import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.AbstractGenerator
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.generator.IGeneratorContext
import dk.sdu.mdsd.arduinoDSL.Node
import dk.sdu.mdsd.arduinoDSL.Component
import java.util.HashMap
import java.util.ArrayList
import dk.sdu.mdsd.arduinoDSL.Rule
import dk.sdu.mdsd.arduinoDSL.Attribute
import dk.sdu.mdsd.arduinoDSL.NumberLiteral
import dk.sdu.mdsd.arduinoDSL.State
import dk.sdu.mdsd.arduinoDSL.Delta
import java.util.HashSet
import dk.sdu.mdsd.arduinoDSL.Assignment
import dk.sdu.mdsd.arduinoDSL.BooleanLiteral
import dk.sdu.mdsd.arduinoDSL.Comparison
import dk.sdu.mdsd.arduinoDSL.NumberExpressionBlock
import dk.sdu.mdsd.arduinoDSL.BooleanExpressionBlock
import dk.sdu.mdsd.arduinoDSL.Plus
import dk.sdu.mdsd.arduinoDSL.Minus
import dk.sdu.mdsd.arduinoDSL.Mult
import dk.sdu.mdsd.arduinoDSL.Div
import dk.sdu.mdsd.arduinoDSL.Mod
import dk.sdu.mdsd.arduinoDSL.AndOr
import dk.sdu.mdsd.arduinoDSL.BooleanOperator
import dk.sdu.mdsd.arduinoDSL.CompareOperator
import dk.sdu.mdsd.arduinoDSL.BooleanExpression
import dk.sdu.mdsd.arduinoDSL.Equals
import dk.sdu.mdsd.arduinoDSL.NotEquals
import dk.sdu.mdsd.arduinoDSL.GreaterThanEquals
import dk.sdu.mdsd.arduinoDSL.Greater
import dk.sdu.mdsd.arduinoDSL.SmallerThanEquals
import dk.sdu.mdsd.arduinoDSL.Smaller
import dk.sdu.mdsd.arduinoDSL.Or
import dk.sdu.mdsd.arduinoDSL.And
import dk.sdu.mdsd.arduinoDSL.Value
import dk.sdu.mdsd.arduinoDSL.NumberExpression
import dk.sdu.mdsd.arduinoDSL.NodeDefinition
import dk.sdu.mdsd.arduinoDSL.VariableDeclaration
import dk.sdu.mdsd.arduinoDSL.IfStatement
import dk.sdu.mdsd.arduinoDSL.SimpleStatement
import dk.sdu.mdsd.arduinoDSL.ElseIfStatement
import dk.sdu.mdsd.arduinoDSL.ElseStatement
import dk.sdu.mdsd.arduinoDSL.VarRef
import dk.sdu.mdsd.arduinoDSL.Cast

class ArduinoDSLGenerator extends AbstractGenerator  {
	
	val nodeRadioIDs = new HashMap<String, String>();
	val componentIDs = new HashMap<String, Integer>();
	val componentSmoothingObjects = new HashMap<String, Component>();
	val ComponentMappingObjects = new HashMap<String, Component>();
	
	val componentValueNameSet = new HashSet<String>();
	val componentReadingNameSet = new HashMap<String, Integer>();
	val componentValueNameUpdateSet = new HashMap<String, Integer>();
	
	
	
	override doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		nodeRadioIDs.clear();
		componentIDs.clear();
		componentSmoothingObjects.clear();
		ComponentMappingObjects.clear();
		componentValueNameSet.clear();
		componentReadingNameSet.clear();
		componentValueNameUpdateSet.clear();
		
		val nodeDefs = input.allContents.filter(NodeDefinition).toList();
		for(var i = 0; i < nodeDefs.size(); i++) {
			nodeRadioIDs.put(nodeDefs.get(i).node.name,  createID(i));
		}
		
		input.allContents.filter(Node).forEach[components.addAll(it.components)]
		var id = 0;
		for(nodeDef : input.allContents.filter(NodeDefinition).toIterable) {
			for(var i = 0; i < nodeDef.node.components.size; i++) {
				componentIDs.put(nodeDef.node.name + nodeDef.node.components.get(i).name, id++);
				if(nodeDef.node.components.get(i).properties.smoothing !== null){
					componentSmoothingObjects.put(nodeDef.node.name + nodeDef.node.components.get(i).name, nodeDef.node.components.get(i));		
				}
				if(nodeDef.node.components.get(i).properties.map !== null){
					ComponentMappingObjects.put(nodeDef.node.name + nodeDef.node.components.get(i).name + 'Value', nodeDef.node.components.get(i));
				}
			}
		}
		fsa.generateFile("ids.txt", nodeRadioIDs.toString + "\n" + componentIDs.toString());
		input.allContents.filter(NodeDefinition).forEach[createFileAndClean(it, input, fsa)];
	}
	
	
	def dispatch Attribute[] getAttributes(NumberExpression exp) {
		val attributes = new ArrayList<Attribute>()
		getAttributeRecursive(exp, attributes)
		
		return attributes
	}
	
	
	def dispatch Attribute[] getAttributes(BooleanExpression exp) {
		val attributes = new ArrayList<Attribute>()
		getAttributeRecursive(exp, attributes)
		
		return attributes
	}
	
	def getAttributeRecursive(Object x, ArrayList<Attribute> list){
		switch x {
			AndOr: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Comparison: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Plus: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Minus:{
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Mult: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Div: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			Mod: {
				getAttributeRecursive(x.left, list)
				getAttributeRecursive(x.right, list)
			}
			BooleanExpressionBlock: {
				getAttributeRecursive(x.block, list)
			}
			NumberExpressionBlock: {
				getAttributeRecursive(x.block, list)
			}
			Attribute: {
				list.add(x)
			}
			Delta: {
				list.add(x.attr)
			}
			default: {
				// do nothing
			}
		}
	}
	

	
	
	
	def String generateAttributeComponentIdConditions(Attribute[] attributes){
		val sb = new StringBuilder()
		var i = 0
		for (attribute : attributes) {
			sb.append('''id.intval == «componentIDs.get(attribute.name.name + attribute.component.name)»«if (i != attributes.length-1) " || " else "" »''')
			i++
		}
		
		return sb.toString()
	}
	
	def String generateAttributeComponentIds(Attribute[] attributes){
		val sb = new StringBuilder()
		var i = 0
		for (attribute : attributes) {
			sb.append('''«componentIDs.get(attribute.name.name + attribute.component.name)»''')
			i++
		}
		return sb.toString()
	}
	
	def createID(int i) {
		if(i == 0){
			return "00";
		} else if(i < 6) {
			var first = i % 6;
			return "0" + first;
		} else if(i < 156){
			var id = "0" + ((((i - 6)/25)<<0) ) + (((i-1)%5) +1) + (((((i-1)/5 << 0)-1)%5) +1)
			if(id.charAt(1) == '0'.charAt(0)){
				return id.substring(1);
			}
			return id
		} else {
			throw new Exception("Input out of range, more than 156 nodes are not supported");
		}
	}
	
	def createFileAndClean(NodeDefinition nodeDef, Resource input, IFileSystemAccess2 fsa) {
		fsa.generateFile(nodeDef.node.name + ".ino", generateNodeFile(nodeDef.node, input, fsa))
		this.componentValueNameSet.clear
		this.componentReadingNameSet.clear
		this.componentValueNameUpdateSet.clear
		
	}
	
	def CharSequence generateNodeFile(Node node, Resource input, IFileSystemAccess2 fsa) '''
	// Generated file, do not edit
	// «node.name»
	#include <SPI.h>
	#include <RF24Network.h>
	#include <RF24.h>
	
	typedef union {
	    unsigned char byteval[4];
	    float floatval;
	} FloatByte;
			
	typedef union {
	    unsigned char byteval[2];
	    int intval;
	} IntByte;
	
	void writeBuffer(FloatByte value, char buf[6]){
	  buf[2] = value.byteval[0];
	  buf[3] = value.byteval[1];
	  buf[4] = value.byteval[2];
	  buf[5] = value.byteval[3];
	}
	
	void writeBuffer(IntByte value, char buf[6]){
	  buf[0] = value.byteval[0];
	  buf[1] = value.byteval[1];
	}
	
	float mapfloat(float x, float in_min, float in_max, float out_min, float out_max)
	{
	 return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
	}
	
	//Radio variables
	RF24 radio(7,8);
	RF24Network network(radio);
	const uint16_t this_node = «nodeRadioIDs.get(node.name)»;
	
	//local outputComponents
	«FOR component : node.components»
	int «component.name»Pin = «IF component.properties.type.equals('analog')»A«ENDIF»«component.properties.pin»;
	«IF component.properties.io == "input"»
		const long «component.name»Rate = «component.properties.rate?.value»;
		long «component.name»LastTransfer = 0;
	«ENDIF»
	
	«ENDFOR»
	
	//Incoming components
	«FOR rule : input.allContents.filter(Rule).toIterable»
«««		«IF rule.body.assignment.findFirst[it.attribute.name.name == node.name] !== null»
			«IF getAttributes(rule.condition).length > 0 »
				«FOR attribute : getAttributes(rule.condition)»
					««« The IF statement below is because we don't want to write anything. Otherwise it would write true/false in the file
					«IF componentValueNameSet.add(attribute.component.name + componentIDs.get(attribute.name.name + attribute.component.name) + 'Value')»«ENDIF»
					«IF attribute.component.properties.smoothing !== null»	
						«IF componentReadingNameSet.put(attribute.component.name + componentIDs.get(attribute.name.name + attribute.component.name), componentSmoothingObjects.get(attribute.name.name + attribute.component.name).properties.smoothing.value.intValue-1) === null»«ENDIF»	
					«ENDIF»				
				«ENDFOR»
			«ENDIF»
«««		«ENDIF»
	«ENDFOR»
	«FOR variable : componentValueNameSet»
		float «variable»;
	«ENDFOR»
	«FOR variable : componentReadingNameSet.entrySet»
	
	float «variable.key»Readings[«variable.value»];
	int «variable.key»ReadIndex = 0;
	int «variable.key»ReadTotal = 0;
	float «variable.key»ReadAverage = 0;

	«ENDFOR»
	«FOR rule : input.allContents.filter(Rule).toIterable»
		bool b«generateAttributeComponentIds(getAttributes(rule.condition))» = false;
	«ENDFOR»
	
	void setup() {
		//Radio
		radio.begin();
		network.begin(90, this_node);
		«FOR component : node.components.filter[it.properties.type == "digital" || it.properties.io == "output"]»
			 pinMode(«component.name»Pin, «component.properties.io.toUpperCase»);
		«ENDFOR»
		«FOR variable : componentReadingNameSet.entrySet»

			for(int reading = 0; reading <= sizeof(«variable.key»)/sizeof(float); reading++) {
				«variable.key»[reading] = 0;
			}

		«ENDFOR»
	}
	
	void loop() {
		network.update();
		«IF input.allContents.filter(Rule).toIterable.exists[it.body.statements.filter(Assignment).findFirst[it.attribute.name.name == node.name] !== null]»
		
		««« READ INCOMING RADIO
while (network.available()) {
			RF24NetworkHeader header;
			char buff[6];
			network.read(header, &buff, sizeof(buff));
			
			IntByte id;
			id.byteval[0] = buff[0];
		    id.byteval[1] = buff[1];
		    
		    FloatByte value;
		    value.byteval[0] = buff[2];
		    value.byteval[1] = buff[3];
		    value.byteval[2] = buff[4];
		    value.byteval[3] = buff[5];
		    
		««« Determine which incoming component and set its value
			«FOR rule : input.allContents.filter(Rule).toIterable»
				«IF rule.body.statements.filter(Assignment).findFirst[it.attribute.name.name == node.name] !== null»
					«IF getAttributes(rule.condition).length > 0 »
						«FOR attribute : getAttributes(rule.condition)»
							««« The IF statement below is because we don't want to write anything. Otherwise it would write true/false in the file
							«IF componentValueNameUpdateSet.put(attribute.component.name + componentIDs.get(attribute.name.name + attribute.component.name) + 'Value', componentIDs.get(attribute.name.name + attribute.component.name)) === null»«ENDIF»				
						«ENDFOR»
					«ENDIF»
				«ENDIF»
			«ENDFOR»
			«FOR variable : componentValueNameUpdateSet.entrySet»
				if(id.intval == «variable.value») {
					«variable.key» = value.floatval;
				}
			«ENDFOR»
			

		««« RULES
		«FOR rule : input.allContents.filter(Rule).toIterable»
			«IF rule.body.statements.filter(Assignment).findFirst[it.attribute.name.name == node.name] !== null»
				«IF getAttributes(rule.condition).length > 0»	if («generateAttributeComponentIdConditions(getAttributes(rule.condition))») {«ENDIF»
					«IF rule.type.equals('when')»
						if («rule.condition.generateExpressions») {
						«FOR statement : rule.body.statements»
«««							her
							«generateStatements(statement)»
						«ENDFOR»
						}
					«ELSE»
						if ((«rule.condition.generateExpressions») && b«generateAttributeComponentIds(getAttributes(rule.condition))» == false) {
							b«generateAttributeComponentIds(getAttributes(rule.condition))» = true;
							«FOR myAssignment : rule.body.statements.filter(Assignment)»
								«myAssignment.attribute.component.properties.type»Write(«myAssignment.attribute.component.name»Pin, «valueToString(myAssignment)»);
							«ENDFOR»
						} else {
							b«generateAttributeComponentIds(getAttributes(rule.condition))» = false;
						}
					«ENDIF»
					}
			«ENDIF»
		«ENDFOR»
		
		}
		«ENDIF»
		//Sample and Transmit sensor data
		«FOR component : node.components.filter[it.properties.io == "input"]»
			if(millis() > «component.name»LastTransfer + «component.name»Rate){
				char buff[6];
				IntByte id;
				id.intval = «componentIDs.get(node.name + component.name)»;
				writeBuffer(id, buff);
				
				FloatByte value;
				«IF componentSmoothingObjects.containsKey(node.name + component.name)»
					«node.name+component.name»ReadTotal = «node.name+component.name»ReadTotal - «node.name+component.name»Readings[«node.name+component.name»ReadIndex];
				«ENDIF»
				«IF component.properties.map !== null»
					value.floatval = mapfloat(«component.properties.type»Read(«component.name»Pin), «component.properties.map.in.low», «component.properties.map.in.high», «component.properties.map.out.low», «component.properties.map.out.high»);
				«ELSE»
					value.floatval = «component.properties.type»Read(«component.name»Pin);
				«ENDIF»
				«IF componentSmoothingObjects.containsKey(node.name + component.name)»

					«node.name+component.name»Readings[«node.name+component.name»ReadIndex] = value.floatVal;
					«node.name+component.name»ReadTotal = «node.name+component.name»ReadTotal + «node.name+component.name»Readings[«node.name+component.name»ReadIndex];
					«node.name+component.name»ReadIndex = «node.name+component.name»ReadIndex + 1;
					
					if («node.name+component.name»ReadIndex >= (sizeof(«node.name+component.name»)/sizeof(float))) {
						«node.name+component.name»ReadIndex = 0;
					}
					«node.name+component.name»ReadAverage = «node.name+component.name»ReadTotal / (sizeof(«node.name+component.name»)/sizeof(float));
					value.floatval = «node.name+component.name»ReadAverage;
				«ENDIF»
				«FOR componentName : ComponentMappingObjects.entrySet»
					«componentName.key» = mapFloat(«componentName.key», «ComponentMappingObjects.get(componentName.key).properties.map.in.low», «ComponentMappingObjects.get(componentName.key).properties.map.in.high», «ComponentMappingObjects.get(componentName.key).properties.map.out.low», «ComponentMappingObjects.get(componentName.key).properties.map.in.high»);
				«ENDFOR»
				
				writeBuffer(value, buff);
				
				«val exist = new HashSet<Node>»
				«FOR rule : input.allContents.filter(Rule).filter[getAttributes(it.condition).map[it.component].contains(component)].toIterable»
					«FOR assignment : rule.body.statements.filter(Assignment)» ««« .map[it.attribute].filter[exist.add(it.name)]»
						«IF exist.add(assignment.attribute.name)»
							«IF !assignment.attribute.name.name.equals(node.name)»
								forceSend(«nodeRadioIDs.get(assignment.attribute.name.name)», buff, sizeof(buff));
							«ELSE»
								«IF assignment.attribute.component.properties.type == "analog"»
									analogWrite(«assignment.attribute.component.name»Pin, «valueToString(assignment)»);
								«ELSE»
									digitalWrite(«assignment.attribute.component.name»Pin, «valueToString(assignment)»);
								«ENDIF»
							«ENDIF»
						«ENDIF»
					«ENDFOR»
				«ENDFOR»
				
				«component.name»LastTransfer = millis();			 	  		 
			}
		«ENDFOR»
	}
	
	void forceSend(uint16_t addressOfReceiver, char buff[], int bufferLength){
		RF24NetworkHeader header(addressOfReceiver);
		bool ok = false;
		while(!ok){
			ok = network.write(header, buff, bufferLength);
		}
	}
	
	'''
	
	def dispatch String valueToString(Assignment assignment) {	
		var x = assignment.value
		switch x {
			State: {
				var out = assignment.attribute.component.properties.map?.out
				if(out === null) return if (x.value == "on") "1" else "0"
				return if (x == "on") ""+out.high+"" else ""+out.low+""
			}
			Plus: {
				return generateExpressions(x)
			}
			Minus: {
				return generateExpressions(x)
			}
			NumberLiteral: {
				return generateExpressions(x)
			}
			BooleanLiteral: {
				return generateExpressions(x)
			}
			default: {
				// do nothing
			}
		}
	}
	
	
	def String generateStatements(Object stmt){
		switch(stmt) {
			Assignment: stmt.attribute.component.properties.type + 'Write' + '(' + stmt.attribute.component.name + 'Pin, ' + valueToString(stmt) + ');' 
			VariableDeclaration: '''«stmt.type» «stmt.name» = «IF stmt.cast !== null»(«stmt.cast.castType») «ENDIF»«generateStatements(stmt.value)»'''
			VarRef: '''«stmt.ref.name»«IF stmt.value !== null» = «IF stmt.cast !== null»(«stmt.cast.castType») «ENDIF»«generateStatements(stmt.value)»«ELSE»;«ENDIF»'''
			Attribute: stmt.component.name +componentIDs.get(stmt.name.name + stmt.component.name) + "Value;"
			Delta: stmt.attr.component.name +componentIDs.get(stmt.attr.name.name + stmt.attr.component.name) + "Value;"
			Cast: '''(«stmt.castType»'''
			IfStatement: {
				'''if («generateExpressions(stmt.condition)») {
					«FOR st : stmt.statements»
						«generateStatements(st)»
					«ENDFOR»
					}
					«IF stmt.^else !== null»
						«generateStatements(stmt.^else)»
					«ENDIF»
					«IF stmt.elseif !== null»
						«generateStatements(stmt.elseif)»
					«ENDIF»
					'''
			}
			ElseIfStatement: {				
				'''else if («generateExpressions(stmt.condition)») {
					«FOR st : stmt.statements»
						«generateStatements(st)»
					«ENDFOR»
					}
					«IF stmt.^else !== null»
						«generateStatements(stmt.^else)»
					«ENDIF»
					«IF stmt.elseif !== null»
						«generateStatements(stmt.elseif)»
					«ENDIF»
					'''
			}
			ElseStatement: {	
				'''else {
					«FOR st : stmt.statements»
						«generateStatements(st)»
					«ENDFOR»
				}'''
			}
			NumberLiteral: {
				if (stmt.floatVal !== null) {
					return stmt.floatVal + ';'
				} else {
					return Integer.toString(stmt.intVal) + ';'
				}
			}
			BooleanLiteral: Boolean.toString(stmt.value) + ';'	
			default: {
				// do nothing
			}		
		}
	}
	
	
	
	def String generateBooleanOperator(BooleanOperator op) {
		switch(op) {
			Or: '''&&'''
			And: '''||'''
		}
	}
	
	def String generateCompareOperator(CompareOperator op) {
		switch(op) {
			Equals: '''='''
			NotEquals: '''!='''
			GreaterThanEquals: '''>='''
			Greater: '''>'''
			SmallerThanEquals: '''<='''
			Smaller: '''<'''	
			default: {
				// do nothing
			}
		}
	}
		
	def dispatch String generateExpressions(BooleanExpression exp) {
		switch exp {
			AndOr: exp.left.generateExpressions +' '+ exp.operator.generateBooleanOperator +' '+ exp.right.generateExpressions
			Comparison: exp.left.generateExpressions +' '+ exp.operator.generateCompareOperator +' '+ exp.right.generateExpressions
			BooleanExpressionBlock: '('+exp.block.generateExpressions+')'
//			BooleanLiteral: Boolean.toString(exp.value)
			Attribute: exp.component.name +componentIDs.get(exp.name.name + exp.component.name) + "Value"
			Delta: exp.attr.component.name +componentIDs.get(exp.attr.name.name + exp.attr.component.name) + "Value"
			VariableDeclaration: exp.name
			default: {
				//do nothing
			}
		}
	}
	
	def dispatch String generateExpressions(NumberExpression exp) {
		switch exp {	
			Plus: exp.left.generateExpressions + ' + ' + exp.right.generateExpressions
			Minus: exp.left.generateExpressions + ' - ' + exp.right.generateExpressions
			Mult: exp.left.generateExpressions + ' * ' + exp.right.generateExpressions
			Div: exp.left.generateExpressions + ' / ' + exp.right.generateExpressions
			Mod: exp.left.generateExpressions + ' % ' + exp.right.generateExpressions
			NumberExpressionBlock: '('+exp.block.generateExpressions+')'
			NumberLiteral: {
				if (exp.floatVal !== null) {
					return exp.floatVal
				} else {
					return Integer.toString(exp.intVal)
				}
			}
			Attribute: exp.component.name +componentIDs.get(exp.name.name + exp.component.name) + "Value"
			Delta: exp.attr.component.name +componentIDs.get(exp.attr.name.name + exp.attr.component.name) + "Value"
			VariableDeclaration: exp.name
			default: {
				//do nothing
			}
		}
	}
}