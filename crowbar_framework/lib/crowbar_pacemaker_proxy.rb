#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#
# Helper related to clusters
#

module CrowbarPacemakerProxy
  # There's no way to have the core of crowbar not know about clusters. This
  # means that we have a loose dependency (in terms of architecture) on the
  # barclamps defining clusters (eg, Pacemaker) here.

  # Returns: list of available clusters
  def available_clusters
    @available_clusters ||= begin
      clusters = {}
      if defined?(PacemakerServiceObject)
        clusters.merge!(PacemakerServiceObject.available_clusters)
      end
      clusters
    end
  end

  # Returns: name of the barclamp and of the proposal for this cluster
  def cluster_get_barclamp_and_proposal(element)
    result = nil

    if defined?(PacemakerServiceObject)
      result ||= PacemakerServiceObject.cluster_get_barclamp_and_proposal(element)
    end

    result ||= [nil, nil]
  end

  def is_cluster?(element)
    result = false

    if defined?(PacemakerServiceObject)
      result ||= PacemakerServiceObject.is_cluster?(element)
    end

    result
  end

  # Returns: name of the cluster, or nil if it's not a cluster
  def cluster_name(element)
    name = nil

    if defined?(PacemakerServiceObject)
      name ||= PacemakerServiceObject.cluster_name(element)
    end

    name
  end

  def cluster_exists?(element)
    !available_clusters[element].nil?
  end

  # Returns: a list with two things:
  #  - the list of all nodes in items; if item contains clusters, these
  #    clusters will be expanded to the list of nodes they're made of
  #    For instance [ node1, cluster1 ] will be expanded to [node1,
  #    node1-of-cluster1, node2-of-cluster1]
  #  - the list of items that are not nodes but that we failed to expand. This
  #    can be used for knowing that expansion partially failed matters.
  def expand_nodes_for_all(items)
    nodes = []
    failures = []

    items.each do |item|
      expanded = nil

      if is_cluster? item
        if defined?(PacemakerServiceObject)
          expanded = PacemakerServiceObject.expand_nodes(item)
        end

        if expanded.nil?
          failures << item
        else
          nodes.concat(expanded)
        end
      else
        nodes << item
      end
    end

    [nodes, failures]
  end

end
