#/bin/bash

../metis.native -u ../input/metis_varnish_wordpress_nfs.json -c Varnish -s Active -o ../results/varnish_wordpress_nfs_results.txt -ap ../results/varnish_wordpress_nfs_abstract_plan.dot
