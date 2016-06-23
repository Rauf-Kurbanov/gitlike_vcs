(ns gitlike-vcs.core)

; TODO remove if won't use
;(require '[clojure.core.match :refer [match]])
(require '[clojure.set :refer [intersection union]])

; for now let's asume having map <filename file>
; instead of directory tree
(def empty-snapshot
  {:files {}}
  )

(def empty-commit
  { :snapshot nil
    :parent-id nil }
  )

(def empty-branch
  { :commit-id nil }
  )

(def empty-repo
  {:head     "master"
   :branches {"master" nil}
   :commits []
   }
  )

(defn commit
  [repo snapshot]
  (let [ {bs :branches cs :commits} repo
        curr-branch    (bs :head)
        new-commit     { :snapshot snapshot
                         :parent-id  (curr-branch :commit-id)}
        new-id         (count cs)
        updated-branch (assoc curr-branch :commit-id new-id)
        ]
    (assoc repo :head updated-branch
                :commits (conj cs new-commit))
    )
  )

; [repo hash]
(defn checkout [repo branch-name]
  (assoc repo :head branch-name)
  )

(defn branch [repo branch-name]
  (let [{h :head bs :branches} repo
        new-branch (bs h)]
    (assoc repo :branches (assoc bs branch-name new-branch))
    )
  )

(def empty-conflict
  {
   :status false
   :conflicting {}                                             ;
   }
  )

(def empty-merge-res
  {
   :status nil
   :conflicts {}
   :resolver nil
   }
  )

(defn succeed-merge
  [file]
  { :status true
    :conflicting {}
    :gimme-resolver (constantly file)
   }
  )

(defn confl-merge
  [from to]
  {
   :status false
   :conflict {:from from :to to}
   :gimme-resolver (fn [resolver] (resolver from to))
   }
  )

; [lca f1 f2] -> [status [resolver] -> file]
(defn three-way-merge-files
  [lca f1 f2]
  (let []
    (cond
      (= lca f1) (succeed-merge f2)
      (= lca f2) (succeed-merge f1)
      (= f1 f2) (succeed-merge f1)
      :else (confl-merge f1 f2)
      )
    )
  )


; Let's assume snapshots always have same files
; [lca from to] -> [status [[resolver] -> commit]]
(defn three-way-merge
  [lca from to new-parent-id]
  (let [ [fl ff ft] (map
                      (fn [c] (get-in c [:snapshot :files]))
                      [lca from to])
        filenames (keys (union lca from to))
        file-status-map (loop [stats {}, names filenames]
                     (let [k (first names)
                           merge-stat (three-way-merge-files (fl k) (ff k) (ft k))
                           ]
                       (if (empty? filenames)
                         stats
                         (recur (assoc stats name merge-stat)
                               (rest names)
                               )
                         )
                       )
                     )
        new-files (fn [super-resolver]
                    (into {}
                          (for [[k v] file-status-map]
                            [k ((v :gimme-resolver) super-resolver)])
                          )
                    )
        gimme-super-resolver (fn [super-resolver]
                               {:snapshot {:files (new-files super-resolver)}
                                :parent-id   new-parent-id
                                }
                               )
        ]
    {
     :statusmap file-status-map
     :gimme-super-resolver gimme-super-resolver
     }
    )
  )

; merge :: [repo branch-name] -> [status ([resolver] -> repo)]
(defn merge [repo branch-name]
  ; TODO make distinct function for [from to lca]
  ; TODO Eliminate code duplicate
  (let [ {h :head bs :branches cs :commits} repo
        bh-where (branch-history repo branch-name)
        bh-curr (branch-history repo h)
        fromi (last (branch-history repo h))
        toi (last (branch-history repo branch-name))
        lci (last-common bh-where bh-curr)
        [lca from to] (select-keys (repo :commits) [lci fromi toi])
        merge-status (three-way-merge lca from to toi)
        ; TODO gimme-resolver factory
        gimme-super-resolver (fn [super-resolver]
                               (let [ gimme-res (merge-status :gimme-super-resolver)
                                     merge-commit (gimme-res super-resolver)
                                     ]
                                 (assoc repo :branches (dissoc bs h)
                                             :head     branch-name
                                             :commits  (conj cs merge-commit)
                                             )
                                 )
                               )
        ]
    {
     :statusmap merge-status
     :gimme-super-resolver gimme-super-resolver
     }
    )
  )

(declare branch-history)
(declare last-common)

(defn rebase [repo branch-name]
  (let [ {h :head bs :branches cs :commits} repo
        bh-where (branch-history repo branch-name)
        bh-curr (branch-history repo h)
        lc (last-common bh-where bh-curr)
        last-comm-repo { :head branch-name
                        :branches (filter #(<= (% :commit-id) lc) bs)
                        :commits (filter #(<= % lc) cs)
                        }
        where-comms (filter #(> % lc) bh-where)
        curr-comms (filter #(> % lc) bh-curr)
        all-comms (flatten [where-comms, curr-comms])
        ]

        (loop [repo-version last-comm-repo, rest-comms all-comms]
          (if (empty? rest-comms)
            repo-version
            (recur
              (commit last-comm-repo ((rest-comms :head) :snapshot))
              (rest rest-comms)
                   )
            )

          )
    )
  )

(defn sorted-to-set
  [vec]
  (reduce conj #{} vec)
  )

(defn last-common
  "Id of last common ancestor"
  [ids1 ids2]
  (let [ss1 (sorted-to-set ids1)
        ss2 (sorted-to-set ids2)]
    (last (intersection ss1 ss2))
    )
  )

(declare prev-ids)

(defn branch-history
  [repo branch-name]
  (let [ { bs :branches } repo
        last-comm-id (bs branch-name)
        ]
    (prev-ids last-comm-id repo)
    )
  )

(defn history [repo path]
  (let [ {h :head bs :branches} repo
        last-comm-id (get-in bs [h :commit-id])
        commit-ids (prev-ids last-comm-id repo)
        ]
    (map (fn [id] get-in repo (flatten [:commits id :snapshot path])
           )
         commit-ids
         )
    )
  )

; TODO reduce
(defn prev-ids
  [commit-id repo]
  (loop [curr-id commit-id, id-list [commit-id]]
    (let [parent-id (get-in repo [:commits curr-id :parent-id])]
      (if (nil? parent-id)
        id-list
        (recur
          parent-id
          (conj id-list parent-id)
          )
        )
      )
    )
  )