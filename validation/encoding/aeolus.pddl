(define (domain aeolus)
 	(:requirements :typing :adl :equality)

 	(:types
		resource
		port
		node
	)

 	(:predicates

		(initial_resource ?r - resource)
			; intial state of the resource r
    
		(transition ?r1 - resource ?r2 - resource)

		(resource_provides_port ?r - resource ?p - port)
		(resource_requires_port ?r - resource ?p - port)

		(node_resource ?n - node ?r - resource)
		
		(used_node ?n - node)

	)
	
	(:action change_state
		:parameters (?n - node ?r1 - resource ?r2 -resource)
		:precondition (and
			(node_resource ?n ?r1)
			(transition ?r1 ?r2)
			; requirements must be satisfied in the next state
			(forall (?r3 - resource ?n1 - node ?p - port)
				(imply
					(and
						(not (= ?n ?n1))
						(node_resource ?n1 ?r3)
						(resource_requires_port ?r3 ?p)
						(resource_provides_port ?r1 ?p)
						(not (resource_provides_port ?r2 ?p))
					)
					(exists (?r4 - resource ?n2 - node)
						(and
							(node_resource ?n2 ?r4)
							(resource_provides_port ?r4 ?p)
							(not (= ?n ?n2))
						)
					)
				)
			)
			(forall (?p - port)
				(imply
					(and
						(resource_requires_port ?r2 ?p)
					)
					(exists (?r3 - resource ?n1 - node)
						(and
							(node_resource ?n1 ?r3)
							(resource_provides_port ?r3 ?p)
							(not (= ?n ?n1))
						)
					)
				)
			)
			
		)
		:effect (and 
			(not (node_resource ?n ?r1))
			(node_resource ?n ?r2)
		)
	)

 	(:action create_node
		:parameters (?n - node ?r - resource )
		:precondition (and
			(not (used_node ?n))
			(initial_resource ?r)
		)
		:effect (and
			(node_resource ?n ?r)
			(used_node ?n)
		)
	)

	(:action delete_node
		:parameters (?n - node ?r1 - resource)
		:precondition (and
			(node_resource ?n ?r1)
			(forall (?r2 - resource ?n1 - node ?p - port)
				(imply
					(and
					    (node_resource ?n1 ?r2)
					    (not (= ?n ?n1)) 
					    (resource_requires_port ?r2 ?p)
					    (resource_provides_port ?r1 ?p)
					)
					(exists (?r3 - resource ?n2 - node)
						(and
							(node_resource ?n2 ?r3)
							(resource_provides_port ?r3 ?p)
							(not (= ?n ?n2))
						)
					)
				)
			)    
		)
		:effect (and
			(not (node_resource ?n ?r1))
			(not (used_node ?n))
		)
	)

)
