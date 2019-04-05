[@bs.module "emotion-server"]
external renderStylesToString: string => string = "";

type critical = {
  .
  "html": string,
  "css": string,
};

[@bs.module "emotion-server"]
external extractCritical: string => critical = "";

module Fs = {
  [@bs.val] [@bs.module "fs"]
  external mkdirSync:
    (
      string,
      {
        .
        "recursive": bool,
        "mode": int,
      }
    ) =>
    unit =
    "";

  [@bs.val] [@bs.module "fs"]
  external symlinkSync: (string, string) => unit = "";
};

module Rimraf = {
  [@bs.val] [@bs.module "rimraf"] external sync: string => unit = "";
};

Array.length(Sys.argv) > 2 && Sys.argv[2] == "prod"
  ? {
    Links.Cdn.prefix := "https://cdn.codaprotocol.com/v2";
  }
  : ();

let writeStatic = (path, rootComponent) => {
  let rendered =
    extractCritical(ReactDOMServerRe.renderToStaticMarkup(rootComponent));
  Node.Fs.writeFileAsUtf8Sync(
    path ++ ".html",
    "<!doctype html>\n" ++ rendered##html,
  );
  Node.Fs.writeFileAsUtf8Sync(path ++ ".css", rendered##css);
};

let asset_regex = [%re {|/\/static\/blog\/.*{png,jpg,svg}/|}];

let posts = {
  let unsorted =
    Node.Fs.readdirSync("posts")
    |> Array.to_list
    |> List.filter(s => Js.String.endsWith(Markdown.suffix, s))
    |> List.map(fileName => {
         let length =
           String.length(fileName) - String.length(Markdown.suffix);
         let name = String.sub(fileName, 0, length);
         let path = "posts/" ++ fileName;
         let (html, content) = Markdown.load(path);
         let metadata = BlogPost.parseMetadata(content, path);
         (name, html, metadata);
       });

  List.sort(
    ((_, _, metadata1), (_, _, metadata2)) => {
      let date1 = Js.Date.fromString(metadata1.BlogPost.date);
      let date2 = Js.Date.fromString(metadata2.date);
      let diff = Js.Date.getTime(date2) -. Js.Date.getTime(date1);
      if (diff > 0.) {
        1;
      } else if (diff < 0.) {
        (-1);
      } else {
        0;
      };
    },
    unsorted,
  );
};

module Router = {
  type t =
    | File(string, ReasonReact.reactElement)
    | Css_file(string, string)
    | Dir(string, array(t));

  let generateStatic = {
    let rec helper = path =>
      fun
      | File(name, elem) => {
          writeStatic(path ++ "/" ++ name, elem);
        }
      | Css_file(name, content) => {
          Node.Fs.writeFileAsUtf8Sync(path ++ "/" ++ name ++ ".css", content);
        }
      | Dir(name, routes) => {
          let path_ = path ++ "/" ++ name;
          Fs.mkdirSync(path_, {"recursive": true, "mode": 0o755});
          routes |> Array.iter(helper(path_));
        };

    helper("./");
  };
};

let jobOpenings = [|
  ("engineering-manager", "Engineering Manager (San Francisco)."),
  ("product-manager", "Product Manager (San Francisco)."),
  ("community-manager", "Community Manager (San Francisco)."),
  ("senior-frontend-engineer", "Senior Frontend Engineer (San Francisco)."),
  (
    "protocol-reliability-engineer",
    "Protocol Reliability Engineer (San Francisco).",
  ),
  ("protocol-engineer", "Senior Protocol Engineer (San Francisco)."),
  (
    "director-of-business-development",
    "Director of Business Development (San Francisco).",
  ),
  ("developer-advocate", "Developer Advocate (San Francisco)."),
|];

// GENERATE

Rimraf.sync("site");

let blogPage =
  <Page page=`Blog name="blog" extraHeaders={Blog.extraHeaders()}>
    <Wrapped> <Blog posts /> </Wrapped>
  </Page>;

Router.(
  generateStatic(
    Dir(
      "site",
      [|
        Css_file("fonts", Style.Typeface.Loader.load()),
        File(
          "index",
          <Page page=`Home name="index" footerColor=Style.Colors.navyBlue>
            <Home
              posts={List.map(
                ((name, html, metadata)) =>
                  (name, html, (metadata.BlogPost.title, "blog-" ++ name)),
                posts,
              )}
            />
          </Page>,
        ),
        Dir(
          "blog",
          posts
          |> Array.of_list
          |> Array.map(((name, html, metadata)) =>
               File(
                 name,
                 <Page
                   page=`Blog
                   name
                   extraHeaders={Blog.extraHeaders()}
                   footerColor=Style.Colors.gandalf>
                   <Wrapped> <BlogPost name html metadata /> </Wrapped>
                 </Page>,
               )
             )
          |> Array.append([|File("index", blogPage)|]),
        ),
        Dir(
          "jobs",
          jobOpenings
          |> Array.map(((name, _)) =>
               File(
                 name,
                 <Page
                   page=`Jobs
                   name
                   footerColor=Style.Colors.gandalf
                   extraHeaders={Careers.extraHeaders()}>
                   <Wrapped>
                     <CareerPost path={"jobs/" ++ name ++ ".markdown"} />
                   </Wrapped>
                 </Page>,
               )
             ),
        ),
        File(
          "jobs",
          <Page page=`Jobs name="jobs" extraHeaders={Careers.extraHeaders()}>
            <Wrapped> <Careers jobOpenings /> </Wrapped>
          </Page>,
        ),
        File(
          "code",
          <Page page=`Code name="code"> <Wrapped> <Code /> </Wrapped> </Page>,
        ),
        File(
          "testnet",
          <Page
            page=`Testnet name="testnet" extraHeaders={Testnet.extraHeaders()}>
            <Wrapped> <Testnet /> </Wrapped>
          </Page>,
        ),
        File("blog", blogPage),
        File(
          "privacy",
          <Page page=`Privacy name="privacy">
            <RawHtml path="html/Privacy.html" />
          </Page>,
        ),
        File(
          "tos",
          <Page page=`Tos name="tos"> <RawHtml path="html/TOS.html" /> </Page>,
        ),
      |],
    ),
  )
);
Fs.symlinkSync(Node.Process.cwd() ++ "/static", "./site/static");
