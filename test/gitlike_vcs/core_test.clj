(ns gitlike-vcs.core-test
  (:refer-clojure :exclude [merge])
  (:require [clojure.test :refer :all]
            [gitlike-vcs.core :refer :all])
  (:use [clojure.pprint :only (pprint)]))

(def someCommit
  {:parent-id nil
   :snapshot {:files {:file-a "very"
                      :file-b "sophisticated"
                      :file-c "data"}}})

(def commitList
  [{:parent-id nil
     :snapshot {:files {:file-a "very"
                        :file-b "sophisticated"
                        :file-c "data"}}}
   {:parent-id 0
    :snapshot {:files {:file-a "not so"
                       :file-b "sophisticated"
                       :file-c "data"}}}
   {:parent-id 1
    :snapshot {:files {:file-a "not so"
                       :file-b "sophisticated"
                       :file-c "DATA"}}}
   {:parent-id 2
    :snapshot {:files {:file-a "very"
                       :file-b "sophisticated"
                       :file-c ""}}}
   {:parent-id 1
    :snapshot {:files {:file-a "not so"
                       :file-b "sophisticated"
                       :file-c "alternative branch"}}}])

(def test-repo
  {:head     "master"
   :branches {"master"     {:commit-id 3}
              "experiment" {:commit-id 4}}
   :commits  commitList
   })

(deftest checkout-test
  (testing "checkout"
    (let [repo test-repo
          repo (checkout repo "experiment")]
      (is (= (repo :head) "experiment")))))

(deftest commit-test
  ( testing "commit"
    (let [repo test-repo
          sn1 {:files {:file-a "very"
                       :file-b "sop"
                       :file-c ""}}
          sn2 {:files {:file-a "very"
                       :file-b "sop"
                       :file-c "data"}}
          repo (commit repo sn1)
          repo (commit repo sn2)
          {h :head bs :branches cs :commits} repo
          last-cid (get-in bs [h :commit-id])
          lastc (cs last-cid)]
      (is (and
            (= "master" h)
            (= 7 (count cs))
            (= 6 last-cid)
            (= lastc
               {:snapshot {:files {:file-a "very"
                                   :file-b "sop"
                                   :file-c "data"}}
                :parent-id 5}))))))

(deftest branch-test
  (let
    [repo test-repo
     repo (branch repo "new-branch")
     {h :head bs :branches cs :commits} repo]
    (is (= (bs "new-branch")
           (bs h)))))

(deftest history-test
  (let [repo test-repo
        repo (checkout repo "experiment")
        path [:files]
        hist (history repo path)
        ]
    (is (= hist
           [{:commit-id 4,
             :path-sh
                        {:file-a "not so"
                         :file-b "sophisticated"
                         :file-c "alternative branch"}}
             {:commit-id 1
              :path-sh {:file-a "not so"
                        :file-b "sophisticated"
                        :file-c "data"}}
             {:commit-id 0,
              :path-sh {:file-a "very"
                        :file-b "sophisticated"
                        :file-c "data"}}]))))

(run-tests)


