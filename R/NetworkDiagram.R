# Author: Kevin See
# Purpose: Create network diagram for PITcleanr
# Created: 2/4/2019
# Last Modified: 2/21/2019
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(stringr)
library(PITcleanr)
library(magrittr)
library(tidygraph)
library(ggraph)
library(igraph)
library(netplot)

theme_set(theme_bw())

#-----------------------------------------------------------------
# build configuration file
org_config = buildConfig()

# customize some nodes based on DABOM framework
my_config = org_config %>%
  mutate(Node = ifelse(SiteID %in% c('VC2', 'VC1', 'LTR', 'MTR', 'UTR'),
                       SiteID,
                       Node),
         Node = ifelse(SiteID == 'SC2',
                       'SC2B0',
                       Node),
         Node = ifelse(SiteID %in% c('CROTRP',
                                     'CRT',
                                     'REDTRP',
                                     'REDR',
                                     'RRT'),
                       'SC2A0',
                       Node),
         Node = ifelse(SiteID == 'AFC',
                       ifelse(grepl('MAINSTEM', AntennaGroup),
                              'AFCB0',
                              'AFCA0'),
                       Node),
         Node = ifelse(SiteID == 'HBC',
                       'HYCA0',
                       Node),
         Node = ifelse(SiteID %in% c('TUCH', 'TFH'),
                       'TUCH',
                       Node),
         Node = ifelse(SiteID == 'MCCA',
                       'STR',
                       Node),
         Node = ifelse(SiteID == 'CARMEC',
                       'CRCA0',
                       Node),
         Node = ifelse(SiteID == 'BIG2C',
                       'TAYA0',
                       Node),
         Node = ifelse(SiteID == 'WIMPYC',
                       'WPCA0',
                       Node),
         Node = str_replace(Node, '^BTC', 'BTL'),
         Node = ifelse(SiteID %in% c('YANKFK', 'CEY'),
                       'YFKA0',
                       Node),
         Node = ifelse(SiteID == 'SAWT',
                       'STL',
                       Node),
         Node = ifelse(SiteID == 'LOOH',
                       'LOOKGC',
                       Node),
         Node = ifelse(SiteID == 'RPDTRP',
                       'RAPH',
                       Node),
         Node = ifelse(SiteID == 'CHARLC',
                       'CCAB0',
                       Node),
         Node = ifelse(Node == 'KEN',
                       'KENB0',
                       Node),
         Node = ifelse(Node == 'HYC',
                       'HYCB0',
                       Node),
         Node = ifelse(Node == 'YFK',
                       'YFKB0',
                       Node),
         Node = ifelse(Node == 'LLR',
                       'LLRB0',
                       Node),
         Node = ifelse(Node == 'LRW',
                       'LRWB0',
                       Node),
         Node = ifelse(SiteID == '18M',
                       str_replace(Node, '18M', 'HEC'),
                       Node)) %>%
  distinct()

# combine all A0 and B0 nodes into single nodes
my_config %<>%
  mutate(Node = ifelse(grepl('A0$', Node) | grepl('B0$', Node),
                       SiteID,
                       Node)) %>%
  distinct()


# build network of nodes
site_df = writeLGRNodeNetwork()

# remove some sites that have been combined with others (see the modifications to the configuration file)
site_df = site_df %>%
  filter(!SiteID %in% c('TFH',
                        'MCCA',
                        'WIMPYC',
                        'YANKFK', 'CEY',
                        'SAWT',
                        'LOOH',
                        'CARMEC',
                        'BIG2C',
                        'RPDTRP'))

# build parent-child table
spp = c('Chinook', 'Steelhead')[1]
yr = 2018
parent_child = createParentChildDf(site_df,
                                   my_config,
                                   startDate = ifelse(spp == 'Chinook',
                                                      paste0(yr, '0301'),
                                                      paste0(yr-1, '0701')))

rootNode = parent_child %>%
  filter(nodeOrder == 1) %>%
  select(ParentNode) %>%
  as.matrix %>%
  as.character


parent_child %<>%
  left_join(site_df %>%
              select(ChildNode = SiteID,
                     Group = Step2) %>%
              mutate_at(vars(Group),
                        funs(ifelse(ChildNode == rootNode,
                                    rootNode,
                                    .)))) %>%
  left_join(my_config %>%
              select(ChildNode = SiteID,
                     lat = Latitude,
                     long = Longitude) %>%
              distinct())

# built table of nodes
nodes = parent_child %>%
  select(ParentNode, ChildNode) %>%
  gather(type, node) %>%
  select(node) %>%
  distinct() %>%
  left_join(parent_child %>%
              rename(node = ChildNode) %>%
              select(-ParentNode)) %>%
  mutate(Group = as.factor(Group),
         Group = fct_relevel(Group, rootNode)) %>%
  arrange(Group, RKM, nodeOrder) %>%
  mutate(index = 1:n()) %>%
  select(index, label = node, everything())

nodes %<>%
  left_join(parent_child %>%
              group_by(label = ParentNode) %>%
              summarise(nChilds = n_distinct(ChildNode)) %>%
              bind_rows(parent_child %>%
                          filter(!ChildNode %in% ParentNode) %>%
                          select(label = ChildNode) %>%
                          mutate(nChilds = 0)) %>%
              mutate(nodeType = if_else(nChilds == 0,
                                        'Terminal',
                                        if_else(nChilds == 1,
                                                'PassThru', 'Branch'))))


# build table of edges (connecting nodes)
edges = parent_child %>%
  filter(ParentNode != ChildNode) %>%
  select(from = ParentNode, to = ChildNode) %>%
  distinct() %>%
  mutate(edgeID = 1:n()) %>%
  gather(direction, label, -edgeID) %>%
  left_join(nodes %>%
              select(index, label)) %>%
  select(-label) %>%
  spread(direction, index) %>%
  select(-edgeID)

# one graph with all sites
myGraph = tbl_graph(nodes = nodes,
                    edges = edges)


#--------------------------------------------------
# tidygraph
# plot it
# myLayout = c('kk')
# myLayout = c('dendrogram')
# myLayout = c('treemap')
myLayout = c('partition')
# myLayout = c('circlepack')
# myLayout = c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds',
#              'randomly', 'fr', 'kk', 'drl', 'lgl')[7]

set.seed(8)
allGr_p = myGraph %>%
  # ggraph(layout = 'igraph',
  #        algorithm = 'nicely') +
  # ggraph(layout = 'igraph',
  #        algorithm = 'fr') +
  ggraph(layout = myLayout) +
  geom_edge_diagonal() +
  # geom_edge_link() +
  geom_node_point(aes(color = Group,
                      shape = nodeType),
                  size = 7) +
  geom_node_label(aes(label = label),
                  # repel = T,
                  size = 2,
                  label.padding = unit(0.1, 'lines'),
                  label.size = 0.1) +
  # geom_node_text(aes(label = label),
  #                 # repel = T,
  #                 size = 2) +
  scale_color_brewer(palette = 'Dark2',
                     guide = 'none') +
  scale_shape_manual(values = c('Branch' = 19,
                                'PassThru' = 18,
                                'Terminal' = 15),
                     guide = 'none') +
  theme_graph(base_family = 'Times') +
  theme(legend.position = 'bottom')
allGr_p


# split by main groups
grList = vector('list', nlevels(nodes$Group) - 1)
names(grList) = levels(nodes$Group)[-1]
for(i in 1:length(grList)) {
  nodeIndex = nodes %>%
    filter(Group %in% c('GRA', levels(nodes$Group)[i+1])) %>%
    select(index) %>%
    distinct %>%
    as.matrix %>%
    as.integer

  nodesToKeep = nodes %>%
    filter(index %in% nodeIndex) %>%
    mutate(newIndex = 1:n())

  edgesToKeep = edges %>%
    filter(to %in% nodeIndex |
             from %in% nodeIndex[-match(1, nodeIndex)]) %>%
    mutate(edgeID = 1:n()) %>%
    gather(direction, index, -edgeID) %>%
    inner_join(nodesToKeep %>%
                 mutate(index = as.character(index)) %>%
                select(newIndex, index, label)) %>%
    select(-index, -label) %>%
    spread(key = direction, value = newIndex) %>%
    select(-edgeID)

  grList[[i]] = tbl_graph(nodes = nodesToKeep %>%
                            select(-index) %>%
                            rename(index = newIndex) %>%
                            select(index, everything()),
                          edges = edgesToKeep) %>%
    mutate(Area = levels(nodes$Group)[i+1])

  rm(nodeIndex, nodesToKeep, edgesToKeep)
}
# plot it
splitGr_p = bind_graphs(grList[[1]],
            grList[[2]],
            grList[[3]],
            grList[[4]],
            grList[[5]]) %>%
  ggraph(layout = 'nicely') +
  geom_edge_link() +
  geom_node_point(aes(color = Group,
                      shape = SiteType),
                  size = 8) +
  geom_node_label(aes(label = label),
                  # repel = T,
                  size = 2,
                  label.padding = unit(0.1, 'lines'),
                  label.size = 0.1) +
  scale_color_brewer(palette = 'Set1') +
  theme_graph(base_family = 'Times') +
  facet_nodes(~ Area,
              scales = 'free') +
  theme(legend.position = 'bottom')

# save the files
ggsave('figures/NetworkGraph_All.pdf',
       allGr_p,
       width = 8,
       height = 8)
ggsave('figures/NetworkGraph_Split.pdf',
       splitGr_p,
       width = 8,
       height = 8)



#--------------------------------------------------
# DiagrammeR
# create graph
myGraph = create_graph() %>%
  add_nodes_from_table(table = nodes,
                       label_col = label) %>%
  add_edges_from_table(table = edges,
                       from_col = from,
                       to_col = to,
                       from_to_map = id_external) %>%
  join_node_attrs(df = get_betweenness(.)) %>%
  join_node_attrs(df = get_degree_total(.)) %>%
  colorize_node_attrs(node_attr_from = Group,
                      node_attr_to = fillcolor,
                      palette = "Set2") %>%
  select_nodes(conditions = label == rootNode) %>%
  set_node_attrs_ws(node_attr = fillcolor,
                    value = 'red') %>%
  clear_selection() %>%
  select_nodes(conditions = SiteType == 'MRR') %>%
  set_node_attrs_ws(node_attr = shape,
                    value = 'triangle') %>%
  clear_selection()

myGraph %>%
  render_graph(title = 'Lower Granite DABOM')
myGraph %>%
  render_graph(layout = 'nicely',
               title = 'Lower Granite DABOM')
myGraph %>%
  render_graph(layout = 'kk',
               title = 'Lower Granite DABOM')

myGraph %>%
  export_graph(file_name = 'figures/NetworkGraphs.pdf',
               title = 'Lower Granite DABOM')

myGraph %>%
  select_nodes_by_degree(
    expressions = "deg < 2") %>%
  get_node_attrs_ws(
    node_attr = label) %>%
  sort() %>%
  unname()

#--------------------------------------------------
# netplot
set.seed(5)
# l = igraph::layout_with_fr(myGraph)
# l = igraph::layout_with_lgl(myGraph)
l = igraph::layout_as_tree(myGraph, flip.y = F)
# l = igraph::layout_with_mds(myGraph)

GRAsites = nplot(myGraph,
                layout = l,
                # vertex.color = viridis::plasma(nlevels(nodes$Group))[nodes$Group],
                vertex.color = RColorBrewer::brewer.pal(nlevels(nodes$Group), 'Set1')[nodes$Group],
                # vertex.color = gray.colors(nlevels(nodes$Group), start = 0.2)[nodes$Group],
                vertex.size = 8,
                vertex.label = nodes$label,
                vertex.label.fontsize = 6,
                edge.curvature = pi/6)
GRAsites

#--------------------------------------------------
# map on landscape
library(sf)
library(ggmap)

#----------------------------------------
# function to download flow lines
#----------------------------------------
# this downloads NHDFlowline data
# I got this from here: https://ryanpeek.github.io/2017-11-05-mapping-with-sf-Part-2/
get_flowlines <- function(streamOrder, mapRange){
  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',streamOrder-1,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',mapRange[2]," ",mapRange[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',mapRange[4]," ",mapRange[3],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')

  destination = file.path(tempdir(),"nhdflowline_network.zip")
  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))

  filePath <- tempdir()
  print("unzipping...")
  unzip(destination, exdir = filePath)

  flowLines <- st_read(filePath, layer = 'nhdflowline_network')

  return(flowLines)
}



node_sf = st_as_sf(nodes,
                   coords = c('long', 'lat'),
                   crs = 4326) %>%
  st_transform(crs = 5070)

# read in HUC4 HUC5 and HUC6 polygons for whole columbia basin
cbHUC4_sf = st_read('/Users/kevin/Dropbox/ISEMP/Data/DesignDocs/WatershedBoundaryLines/Shape/WBDHU8.shp') %>%
  st_transform(st_crs(node_sf))
cbHUC5_sf = st_read('/Users/kevin/Dropbox/ISEMP/Data/DesignDocs/WatershedBoundaryLines/Shape/WBDHU10.shp') %>%
  st_transform(st_crs(node_sf))
cbHUC6_sf = st_read('/Users/kevin/Dropbox/ISEMP/Data/DesignDocs/WatershedBoundaryLines/Shape/WBDHU12.shp') %>%
  st_transform(st_crs(node_sf))

# read in Chinook population boundaries
ckPops = st_read('/Users/kevin/Dropbox/ISEMP/Data/Fish/PopulationBoundaries/CHNK_SPSU_All/CHNK_SPSU_All.shp') %>%
  st_transform(st_crs(node_sf)) %>%
  filter(grepl('Snake River Spring/Summer-run Chinook Salmon ESU', ESU_DPS)) %>%
  mutate_at(vars(ESU_DPS),
            list(fct_drop))

# bb = node_sf %>%
#   st_transform(crs = 4326) %>%
#   st_bbox()
# names(bb) = c('left', 'bottom', 'right', 'top')
# bkgdMap = get_map(location = bb,
#                   maptype = 'terrain',
#                   color = 'bw')

ggplot() +
  geom_sf(data = ckPops,
          aes(fill = ESU_DPS)) +
  geom_sf(data = node_sf,
          aes(color = Group))
