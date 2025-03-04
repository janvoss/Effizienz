from graphviz import Digraph
from IPython.display import display


dot = Digraph()

# Set common attributes for all nodes
dot.attr('node', width='2.5', shape='none')
dot.attr(nodesep='1.0')

dot.attr(label='Effizienz und nachhaltige Entwicklung', labelloc='t', labeljust='c')

# Main nodes A and B
dot.node('A', 'Effizienz', shape='underline', labeljust='c')
dot.node('B', 'nachhaltige Entwicklung', shape='underline', labeljust='c')

# Sub-nodes under A and B with text wrapping
sub_nodes_a = [
    'Bestmöglicher\l Umgang mit\l knappen Ressourcen\l',
    'Präferenzen stehen\l im Zentrum\l',
    'Nachhaltiger Umgang\l mit Ressourcen endogen\l',
    'Neoklassik:\l Anthropozentrisch\l',
    'Beinhaltet keine\l Aussagen zur Verteilung\l',
    'Zeitliche Perspektive:\l statisch vs. dynamisch\l kurz- vs. langfristig\l'
]

sub_nodes_b = [
    'Langfristige\l Nichtüberforderung\l',
    'Menschen: Selbst-\l bestimmte langfristige\l Lebensgestaltung\l Natur: Erhalt von \l Ökosystemen\l',
    'Nachhaltiger Umgang\l mit Ressourcen postuliert\l',
    'Auch\l ökosystemorientiert\l',
    'Beinhaltet\l Verteilungsaussagen\l',
    'Zeitliche Perspektive:\l Langfristorientierung\l'
]

# Create sub-nodes and align them
for i in range(len(sub_nodes_a)):
    a_node = f'A{i+1}'
    b_node = f'B{i+1}'

    dot.node(a_node, sub_nodes_a[i])
    dot.node(b_node, sub_nodes_b[i])

    # Positioning sub-nodes A[i] and B[i] in the same rank
    with dot.subgraph() as s:
        s.attr(rank='same')
        s.node(a_node)
        s.node(b_node)

    # Invisible edges to enforce vertical alignment of sub-nodes under A and B
    if i == 0:
        dot.edge('A', a_node, style='invis')
        dot.edge('B', b_node, style='invis')
    else:
        dot.edge(f'A{i}', a_node, style='invis')
        dot.edge(f'B{i}', b_node, style='invis')

    # Double arrows between corresponding sub-nodes
    dot.edge(a_node, b_node, dir='both')

# Render the graph (this will display the source code)
#print(dot.source)

# To render the graph to a file, use dot.render(filename='output.gv') or dot.view()

# Diagramm im Notebook anzeigen
display(dot)