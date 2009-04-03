/****** Object:  Table [dbo].[Test]    Script Date: 08/29/2008 19:17:39 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Test]') AND type in (N'U'))
DROP TABLE [dbo].[Test]
GO
/****** Object:  Table [dbo].[Test]    Script Date: 08/29/2008 19:17:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Test]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[Test](
	[IdTest] [int] NOT NULL,
	[Str] [nvarchar](max) NULL,
	[Dt] [datetime] NULL,
 CONSTRAINT [PK_Test] PRIMARY KEY CLUSTERED 
(
	[IdTest] ASC
)WITH (IGNORE_DUP_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
END
GO
