# Copyright 2012, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class CmdbExecutor

	##
	# given a proposal_config, add a cmdb_event for each set of role application, to each node.
	# the latest instance for each dependent configuration is queried for its configuration
	# inputs:
	#   - proposal_cofnig - provides the set of node_roles to be applied
	#   - config_deps - an array of config depenencies that need to be provided to each node
	# 	     the format is an array of { :barclamp => "bc_name", :instance => "config_name"}
	def compute_config(cmdb_run, pconfig,config_deps)
		all_config = {}
		checked = {}
		to_check = config_deps.dup
		while to_check.length > 0
			dep = to_check.shift
			checked << dep
			ProposalConfig.find(dep)
		end
		
	end


end
