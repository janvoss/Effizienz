from graphviz import Digraph
from IPython.display import display


dot = Digraph()

# Set common attributes for all nodes

dot.attr('node', width='2.5', shape='box')
dot.attr(nodesep='1.0')
dot.attr(label='Local Control', labelloc='t', labeljust='c', fontsize='20', fontname = 'Times-Bold')

# Increase the vertical distance between title and text
dot.node('invis1', '', shape= 'none', width='0')


# Sub-nodes with text wrapping
sub_nodes_a = [
    'Selbsterhaltung',
    'Ziele setzen',
    'Ziele verfolgen',
    'Zugang zu Ressourcen',
    'Effizienz',
    'Kreativität'
]


# Create sub-nodes and set fontface for A3 and A4
for i in range(len(sub_nodes_a)):
    a_node = f'A{i}'

    if i == 3 or i == 4:
        dot.node(a_node, sub_nodes_a[i], fontname='Times-Bold')
    else:
        dot.node(a_node, sub_nodes_a[i])


    # Invisible edges to enforce vertical alignment of sub-nodes under A and B
    if i > 0:
       dot.edge(f'A{i-1}', a_node, style='invis', weight='10')

dot.edge('invis1', 'A0', style='invis', weight='10')

# Arrows
for i in range(1, 5):
    dot.edge('A5', f'A{i}')

for i in range(2, 4):
    dot.edge('A4', f'A{i}')


dot.node('invis', '', shape= 'none', width='0')
dot.node('caption', label='Darstellung: Jan S. Voßwinkel in enger Anlehnung an Groviec (2022)', shape='none', width = '7', fontsize='10')

with dot.subgraph() as s:
        s.attr(rank='same')
        s.node('caption')
        s.node('invis')

dot.edge(f'A{len(sub_nodes_a) - 1}', 'caption', style='invis')
dot.edge(f'A{len(sub_nodes_a) - 1}', 'invis', style='invis')



# Render the graph (this will display the source code)
# print(dot.source)

# To render the graph to a file, use dot.render(filename='output.gv') or dot.view()
#dot.render(view=True)

# Diagramm im Notebook anzeigen
display(dot)