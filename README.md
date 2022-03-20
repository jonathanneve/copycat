# CopyCat Replication Engine

## Background

CopyCat started as a custom replication solution, created in 2004 for the needs of a customer, which I then refactored and turned into a reusable Delphi component library for creating [Firebird](http://www.firebirdsql.org) database replication systems. It gradually developed into a customizable Delphi replication library supporting multiple database types (Firebird/Interbase, MySQL, Microsoft SQL Server, Oracle, PostegreSQL and NexusDB), and featuring middleware in Java, PHP and Objective-C for iOS. It was used extensively in multiple replication systems of my own, as well as by over 100 customers worldwide.

## CopyCat.cloud

In 2017, I took on an ambitious new project aiming to turn CopyCat from a development library into a cloud-based ETL platform. I performed a lot of work getting the technical implementation working: the new approach was to eliminate reliance on Delphi and instead run the database replication in NodeJS, using Angular to create a customer portal for setting up and configuring database replication systems. The plan was to include connectors in multiple languages that would interface with the cloud API to synchronize data to the cloud and back down to other nodes in the system. The API is not production-ready, but the work in progress is [available here].

In parallel with the technical design and implementation, I spent a long time throughout 2017 and 2018 studying the market and investigating the commercial feasibility and best approaches for the project. It was a learning experience for me, and there is a lot I would do differently next time. But after much time and effort, I was forced to conclude that the project as I envisaged it did not have enough market potential to be viable purely as a SaaS solution, and would only be worthwhile if I were to pivot towards a more hands-on, service-based model. That conclusion led me to abandon the project.

## Open sourcing

Since 2022, the CopyCat project is now being offered as opensource, primarily for the benefit of existing customers who are still using it and who need to ensure continued availability in the future. The codebase is licenced under the MIT licence, which basically means you are free to do whatever you want with it. If you find the project interesting or would like to do something with it, please [drop me a line](mailto:jonneve@gmail.com), as I would love to hear about it. I'm also available for consultancy work on an ad-hoc basis if you need help either implementing or updating a CopyCat replication system.

